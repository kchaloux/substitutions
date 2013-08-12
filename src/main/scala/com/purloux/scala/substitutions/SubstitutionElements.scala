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

    /** Returns a special substitution to get the raw contents of those
     *  substitution elements that are substituted after the fact 
     * 
     *  @param args replacement value mapping
     *  @param substitutor instance used to invoke substitution commands
     */
    def substituteLast(args : Map[String, Any], substitutor : Substitutor): String =
      substitute(args, substitutor)
  }

  /** Plaintext element with no substitution logic */
  case class PlainText(contents : String) extends SubstitutionElement {
    override def substitute(args : Map[String, Any], substitutor : Substitutor): String = contents
  }

  /** Special wrapped plaintext that allows arbitrary expressions that won't be replaced */
  case class EscapeElement(contents : String) extends SubstitutionElement {
    override def substitute(args : Map[String, Any], substitutor : Substitutor): String = s"@<${contents}>"
    override def substituteLast(args : Map[String, Any], substitutor : Substitutor): String = contents
  }

  /** Command or replacement identifier */
  case class Identifier(name : String) extends SubstitutionElement {
    override def substitute(args : Map[String, Any], substitutor : Substitutor): String = ""
  }

  /** Full set of parsed Substitution Elements in sequence */
  case class ElementBlock(elements: Seq[SubstitutionElement]) extends SubstitutionElement {
    override def substitute(args : Map[String, Any], substitutor : Substitutor): String =
      elements.map(_.substitute(args, substitutor)).mkString("")

    override def substituteLast(args : Map[String, Any], substitutor : Substitutor): String =
      elements.map(_.substituteLast(args, substitutor)).mkString("")
  }

  /** List of separate parsed ElementBlocks (such as an Argument List) */
  case class ElementsList(blocks : Seq[ElementBlock]) extends SubstitutionElement {
    override def substitute(args : Map[String, Any], substitutor : Substitutor): String =
      blocks.map(_.substitute(args, substitutor)).mkString("")
  }

  /** Replacement element to be directly substituted with a single string */
  case class Replacement(ident : Identifier) extends SubstitutionElement {
    override def substitute(args : Map[String, Any], substitutor : Substitutor): String =
      args.get(ident.name.toLowerCase) match {
        case Some(value) => value.toString
        case None        => s"@{${ident.name.toLowerCase}}"
      }
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
  }
}