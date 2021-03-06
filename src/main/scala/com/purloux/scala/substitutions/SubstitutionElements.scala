package com.purloux.scala.substitutions

/** Defines possible elements of a text substitution string */
object SubstitutionElements {
  import com.purloux.scala.substitutions.commands.CommandReporting._

  /** Any substitution element with replacement logic */
  trait SubstitutionElement[+A] {

    /** Returns the substituted value of an element given
     *  a map of replacement values and substitution commands
     *
     *  @param args replacement value mapping
     *  @param substitutor instance used to invoke substitution commands
     */
    def substitute(args : Map[String, Any], substitutor : Substitutor): A

    /** Returns the substituted value of an element that has been escaped */
    def substituteEscaped(): String

    /** Returns the substituted values of elements during the final escape-characters
     *  pass of a string to remove all XML-style &amp; escapes
     */
    def substituteEscapedCharacters(): String = 
      substituteEscaped
  }

  /** Plaintext element with no substitution logic */
  case class PlainText(contents : String) extends SubstitutionElement[String] {
    def substitute(args : Map[String, Any], substitutor : Substitutor): String = contents
    def substituteEscaped(): String = contents
  }

  /** Plaintext element for whitespace-only characters with no substitution logic */
  case class WhiteSpace(contents : String) extends SubstitutionElement[String] {
    def substitute(args : Map[String, Any], substitutor : Substitutor): String = contents
    def substituteEscaped(): String = contents
  }

  /** Toplevel block of all elements contained within a template */
  case class WholeText(contents : ElementBlock) extends SubstitutionElement[String] {
    def substitute(args : Map[String, Any], substitutor : Substitutor): String = 
      contents.substitute(args, substitutor).mkString
    
    def substituteEscaped(): String = contents.substituteEscaped
  }

  /** Block of potentially nested escaped elements */
  case class EscapeBlock(elements : Seq[SubstitutionElement[Any]]) extends SubstitutionElement[String] {
    def substitute(args : Map[String, Any], substitutor : Substitutor): String =
      "@<" + elements.map(_.substituteEscaped).mkString + ">"
    
    def substituteEscaped(): String =
      elements.map(_.substituteEscaped).mkString
  }

  /** Inert escape block consisting of just two angle brackets with content that must be matched */
  case class InertAngleBlock(elements : Seq[SubstitutionElement[Any]]) extends SubstitutionElement[String] {
    def substitute(args : Map[String, Any], substitutor : Substitutor): String =
      "<" + elements.map(_.substituteEscaped).mkString + ">"
    
    override def substituteEscaped(): String =
      "<" + elements.map(_.substituteEscaped).mkString + ">"
  }

  /** XML-Style Escape Characters for otherwise potentially meaningful elements */
  case class EscapeCharacter(contents : String) extends SubstitutionElement[String] {
    lazy val symbol = contents.toLowerCase match {
      case "lt" | "less"    => "<"
      case "gt" | "greater" => ">"
      case "br" | "break"   => "\n"
      case "at" | "sigil"   => "@"
      case _ => s"@$contents;"
    }

    lazy val representation = s"@$contents;"

    def substitute(args : Map[String, Any], substitutor : Substitutor): String = representation
    def substituteEscaped(): String = representation
    override def substituteEscapedCharacters(): String = symbol
  }

  /** Command or replacement identifier */
  case class Identifier(name : String) extends SubstitutionElement[String] {
    def substitute(args : Map[String, Any], substitutor : Substitutor): String = ""
    def substituteEscaped(): String = ""
  }

  /** Full set of parsed Substitution Elements in sequence */
  case class ElementBlock(elements: Seq[SubstitutionElement[Any]]) extends SubstitutionElement[Seq[Any]] {
    def substitute(args : Map[String, Any], substitutor : Substitutor): Seq[Any] = {
      elements.map(_.substitute(args, substitutor))
    }

    def substituteEscaped(): String =
      elements.map(_.substituteEscaped).mkString

    override def substituteEscapedCharacters(): String =
      elements.map(_.substituteEscapedCharacters).mkString
  }

  /** List of separate parsed ElementBlocks (such as an Argument List) */
  case class ElementsList(blocks : Seq[ElementBlock]) extends SubstitutionElement[Seq[Seq[Any]]] {
    def substitute(args : Map[String, Any], substitutor : Substitutor): Seq[Seq[Any]] = {
      blocks.map(_.substitute(args, substitutor))
    }

    def substituteEscaped(): String =
      blocks.map(_.substituteEscaped).mkString

    override def substituteEscapedCharacters(): String =
      blocks.map(_.substituteEscapedCharacters).mkString
  }

  /** Replacement element to be directly substituted with a single string */
  case class Replacement(ident : Identifier) extends SubstitutionElement[Any] {
    def substitute(args : Map[String, Any], substitutor : Substitutor): Any =
      args.get(ident.name.toLowerCase) match {
        case Some(value) => value
        case None        => s"@{${ident.name}}"
      }

    def substituteEscaped(): String = "@{" + ident.name + "}"
  }

  /** Unparameterized command replacement to be substituted with one of
   *  several possible strings, provided as an arguments list */
  case class Command(ident : Identifier, contents: ElementsList) extends SubstitutionElement[String] {
    def substitute(args : Map[String, Any], substitutor : Substitutor): String = {
      val replacedContents = contents.blocks.map(_.substitute(args, substitutor).mkString)
      val transform = substitutor.getCommand(ident.name.toLowerCase) match {
        case Some(fn) => fn
        case None     => showCommand(ident.name.toLowerCase)(Seq[String]())
      }
      transform(replacedContents)
    }

    def substituteEscaped(): String = {
      val escapeContents = contents.blocks.map(_.substituteEscaped)
      showCommand(ident.name)(Seq[String]())(escapeContents)
    }
  }

  /** Parametersized command replacement to be substituted with one of
   *  several possible strings, provided as an arguments list, based on
   *  the inputs given to the parameters list */
  case class ParameterizedCommand(ident: Identifier, params: ElementsList, contents: ElementsList)
    extends SubstitutionElement[String]
  {
    def substitute(args : Map[String, Any], substitutor : Substitutor): String = {
      val replacedParams = { 
        params.blocks.map { (block) =>
          val withoutWhiteSpace = block.elements.filterNot(_.isInstanceOf[WhiteSpace])
          if (withoutWhiteSpace.length == 1) {
            withoutWhiteSpace(0).substitute(args, substitutor)
          } else {
            block.elements.map(_.substitute(args, substitutor)).mkString
          }
        }
      }
      
      val replacedContents = contents.blocks.map(_.substitute(args, substitutor).mkString)
      val transform = substitutor.getParamCommand(ident.name.toLowerCase) match {
        case Some(fn) => fn(replacedParams)
        case None     => showCommand(ident.name.toLowerCase)(replacedParams)
      }
      transform(replacedContents)
    }

    def substituteEscaped(): String = { 
      val escapeContents = contents.blocks.map(_.substituteEscaped)
      val escapeParams = params.blocks.map(_.substituteEscaped)
      showCommand(ident.name)(escapeParams)(escapeContents)
    }
  }
}