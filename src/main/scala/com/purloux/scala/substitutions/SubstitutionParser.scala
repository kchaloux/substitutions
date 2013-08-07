package com.purloux.scala.substitutions
import scala.util.parsing.combinator.RegexParsers
import scala.language.postfixOps

/** Functions for parsing the logical elements of a
 *  marked up replacement string
 */
object SubstitutionParser extends RegexParsers {
  import SubstitutionElements._
  import util.matching.Regex
  override val skipWhitespace = false

  // Basic syntax elements and Regular Expressions
  private val sigil       = "@"
  private val dash        = "-"
  private val delimList   = "|"
  private val openList    = "["
  private val closeList   = "]"
  private val openBrace   = "{"
  private val closeBrace  = "}"
  private val delimParam  = ","
  private val openParam   = "("
  private val closeParam  = ")"
  private val start       = "^".r
  private val end         = "$".r
  private val any         = ".*".r
  private val space       = "\\s*".r
  private val ident       = "[\\w]+".r

  private def quote(elems : String*) : String     = "\\Q" + elems.mkString + "\\E"
  private def withAny(elems : String*) : Regex    = ("["  + elems.mkString + "]").r
  private def withoutAny(elems : String*) : Regex = ("[^" + elems.mkString + "]").r

  def commands        = ( replacement | paramCommand | command )
  def plainElement    = ( commands | plainText )
  def listElement     = ( commands | plainListText )
  def paramElement    = ( commands | plainParamText )
  def safeSigil       = withAny(sigil) <~ not(withAny(openBrace))
  def safeDash        = ((start | withoutAny(openBrace)) ~
      withAny(dash) ~ (withoutAny(closeBrace) | end))

  /** Convert a list of strings or characters into a PlainText substitution element */
  def listToPlainText(result: List[java.io.Serializable]) : PlainText = result match {
    case (elems) => PlainText(elems.map {
      _ match {
        case (l~r) => Seq(l, r).mkString("")
        case (any) => any
      }
    }.mkString(""))
  }

  /** Match a body of text consisting of a block of SubstitutionElements */
  def wholeText : Parser[ElementBlock] =
    start ~> (plainElement *) <~ end ^^ { ElementBlock }

  /** Match a single uninterrupted PlainText SubstitutionElement */
  def plainText : Parser[PlainText] =
    rep1 (withoutAny(sigil) | safeSigil ) ^^ { listToPlainText }

  /** Match a single uninterrupted PlainText SubstitutionElement
   *  excluding special characters designated for lists, delimiters and commands
   */
  def plainListText : Parser[PlainText] = {
    rep1 ( withoutAny(sigil, quote(openList, closeList, delimList)) | safeSigil ) ^^
      { listToPlainText }
  }

  /** Match a single uninterrupted PlainText SubstitutionElement
   *  excluding special characters designated for parameter lists, delimiters and commands
   */
  def plainParamText : Parser[PlainText] = {
    (rep1 ( withoutAny(sigil, quote(openParam, closeParam, delimParam)) | safeSigil )) ^^
      { listToPlainText }
  }

  /** Match a parameter list comprising any number of elements */
  def paramList = {
    openParam ~> ( repsep((paramElement *), delimParam) ) <~ closeParam ^^
      { case(blocks) => ElementsList(blocks.map(ElementBlock)) }
  }

  /** Match an argument list comprising any number of elements */
  def argumentList = {
    openList ~> ( repsep((listElement *), delimList) ) <~ closeList ^^
      { case(blocks) => ElementsList(blocks.map(ElementBlock)) }
  }

  /** Match a single Replacement SubstitutionElement with one Identifier */
  def replacement : Parser[Replacement] = {
    (sigil ~ openBrace ~ space ~> ident <~ space <~ closeBrace) ^^
      { case (id) => Replacement(Identifier(id)) }
  }

  /** Match a single Command SubstitutionElement with an argument list */
  def command : Parser[Command] = {
    sigil ~ openBrace ~ space ~> ident ~ space ~ argumentList <~ space ~ closeBrace ^^
      { case (id ~ s ~ list) => Command(Identifier(id), list) }
  }

  /** Match a single ParameterizedCommand SubstitutionElement with a
   *  parameter list and an argument list
   */
  def paramCommand : Parser[ParameterizedCommand] = {
    ((sigil ~ openBrace ~ space ~> ident ~ space ~ paramList ~
        space ~ opt(argumentList) <~ space ~ closeBrace)) ^^
      {
        case (id ~ s1 ~ params ~ s2 ~ Some(list)) =>
          ParameterizedCommand(Identifier(id), params, list)
        case (id ~ s1 ~ params ~ s2 ~ None) =>
          ParameterizedCommand(Identifier(id), params, ElementsList(Seq[ElementBlock]()))
      }
  }
}