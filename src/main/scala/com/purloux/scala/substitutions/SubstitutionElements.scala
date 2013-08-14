package com.purloux.scala.substitutions

/** Defines possible elements of a text substitution string */
object SubstitutionElements {
  import com.purloux.scala.substitutions.commands.ErrorReporting

  /** Any substitution element with replacement logic */
  sealed abstract class SubstitutionElement {

    /** Returns the substituted value of an element given
     *  a map of replacement values and substitution commands
     *
     *  @param args replacement value mapping
     *  @param substitutor instance used to invoke substitution commands
     */
    def substitute(args : Map[String, Any], substitutor : Substitutor): String

    /** Returns the substituted value of an element that has been escaped
     * 
     *  @param args replacement value mapping
     *  @param substitutor instance used to invoke substitution commands
     */
    def substituteEscape(): String
  }

  /** Plaintext element with no substitution logic */
  case class PlainText(contents : String) extends SubstitutionElement {
    override def substitute(args : Map[String, Any], substitutor : Substitutor): String = contents
    override def substituteEscape(): String = contents
  }

  /** Block of potentially nested escaped elements */
  case class EscapeBlock(elements : Seq[SubstitutionElement]) extends SubstitutionElement {
    override def substitute(args : Map[String, Any], substitutor : Substitutor): String =
      "@<" + elements.map(_.substituteEscape).mkString + ">"
    
    override def substituteEscape(): String =
      elements.map(_.substituteEscape).mkString
  }

  /** Inert block of escape elements that may be nested within a top-level escape block */
  case class InertEscapeBlock(elements : Seq[SubstitutionElement]) extends SubstitutionElement {
    override def substitute(args : Map[String, Any], substitutor : Substitutor): String =
      "@<" + elements.map(_.substituteEscape).mkString + ">"
    
    override def substituteEscape(): String =
      "@<" + elements.map(_.substituteEscape).mkString + ">"
  }

  /** Command or replacement identifier */
  case class Identifier(name : String) extends SubstitutionElement {
    override def substitute(args : Map[String, Any], substitutor : Substitutor): String = ""
    override def substituteEscape(): String = ""
  }

  /** Full set of parsed Substitution Elements in sequence */
  case class ElementBlock(elements: Seq[SubstitutionElement]) extends SubstitutionElement {
    override def substitute(args : Map[String, Any], substitutor : Substitutor): String =
      elements.map(_.substitute(args, substitutor)).mkString

    override def substituteEscape(): String =
      elements.map(_.substituteEscape).mkString
  }

  /** List of separate parsed ElementBlocks (such as an Argument List) */
  case class ElementsList(blocks : Seq[ElementBlock]) extends SubstitutionElement {
    override def substitute(args : Map[String, Any], substitutor : Substitutor): String =
      blocks.map(_.substitute(args, substitutor)).mkString

    override def substituteEscape(): String =
      blocks.map(_.substituteEscape).mkString
  }

  /** Replacement element to be directly substituted with a single string */
  case class Replacement(ident : Identifier) extends SubstitutionElement {
    override def substitute(args : Map[String, Any], substitutor : Substitutor): String =
      args.get(ident.name.toLowerCase) match {
        case Some(value) => value.toString
        case None        => s"@{${ident.name.toLowerCase}}"
      }

    override def substituteEscape(): String = "@{" + ident.name + "}"
  }

  /** Unparameterized command replacement to be substituted with one of
   *  several possible strings, provided as an arguments list */
  case class Command(ident : Identifier, contents: ElementsList) extends SubstitutionElement {
    override def substitute(args : Map[String, Any], substitutor : Substitutor): String = {
      val replacedContents = contents.blocks.map(_.substitute(args, substitutor))
      val transform = substitutor.getCommand(ident.name.toLowerCase) match {
        case Some(fn) => fn
        case None     => ErrorReporting.showCommand(ident.name.toLowerCase)(Seq[String]())
      }
      transform(replacedContents)
    }

    override def substituteEscape(): String = {
      val escapeContents = contents.blocks.map(_.substituteEscape)
      ErrorReporting.showCommand(ident.name)(Seq[String]())(escapeContents)
    }
  }

  /** Parametersized command replacement to be substituted with one of
   *  several possible strings, provided as an arguments list, based on
   *  the inputs given to the parameters list */
  case class ParameterizedCommand(ident: Identifier, params: ElementsList, contents: ElementsList)
    extends SubstitutionElement
  {
    override def substitute(args : Map[String, Any], substitutor : Substitutor): String = {
      val replacedContents = contents.blocks.map(_.substitute(args, substitutor))
      val replacedParams = params.blocks.map(_.substitute(args, substitutor))
      val transform = substitutor.getParamCommand(ident.name.toLowerCase) match {
        case Some(fn) => fn(replacedParams)
        case None     => ErrorReporting.showCommand(ident.name.toLowerCase)(replacedParams)
      }
      transform(replacedContents)
    }

    override def substituteEscape(): String = { 
      val escapeContents = contents.blocks.map(_.substituteEscape)
      val escapeParams = params.blocks.map(_.substituteEscape)
      ErrorReporting.showCommand(ident.name)(escapeParams)(escapeContents)
    }
  }
}