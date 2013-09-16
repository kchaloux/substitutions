package com.purloux.scala.substitutions.parsers
import scala.util.parsing.combinator.RegexParsers
import scala.language.postfixOps

/** Parsing engine for a marked up substitution string */
object SubstitutionParser extends RegexParsers with ParserPatterns {
  import com.purloux.scala.substitutions.SubstitutionElements._
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
  private val openAngle   = "<"
  private val closeAngle  = ">"
  private val ident       = "[\\w-]+".r
  private val singleSpace = " "
  private val lineFeed    = "\n"
  private val tabChar     = "\t"
  private val newLine     = "(\r)?\n".r

  private def commands        = ( replacement | paramCommand | command )
  private def anyElement      = ( commands | escapeBlock | plainText | whiteSpaceText )
  private def listElement     = ( commands | escapeBlock | plainListText | whiteSpaceText )
  private def argumentElement  = ( commands | escapeBlock | plainArgumentText | whiteSpaceText )
  private def anyEscapeElement = ( escapeText | inertAngleBlock )
  private def safeCommandSigil = withAny(sigil) <~ not(withAny(openBrace))
  private def safeEscapeSigil  = withAny(sigil) <~ not(withAny(openAngle))
  private def safeSigil        = withAny(sigil) <~ not(withAny(openBrace) | withAny(openAngle))

  /** Match a body of text consisting of a block of SubstitutionElements */
  def wholeText : Parser[WholeText] =
    start ~> (anyElement *) <~ end ^^ { case(elements) => WholeText(ElementBlock(elements)) }

  /** Match a single uninterrupted PlainText SubstitutionElement */
  def plainText : Parser[PlainText] =
    rep1 (withoutAny(sigil, singleSpace, lineFeed, tabChar) | safeSigil ) ^^
      { case (contents) => PlainText(listToString(contents)) }

  /** Match a single uninterrupted WhiteSpace substitutionElement */
  def whiteSpaceText : Parser[WhiteSpace] =
    rep1 (singleSpace | newLine | tabChar) ^^
      { case (contents) => WhiteSpace(listToString(contents)) }

  /** Match a single uninterrupted PlainText SubstitutionElement
   *  excluding special characters designated for lists, delimiters and commands
   */
  def plainListText : Parser[PlainText] = {
    rep1 ( withoutAny(sigil, singleSpace, lineFeed, tabChar,
           quote(openList, closeList, delimList)) | safeSigil ) ^^
      { case(contents) => PlainText(listToString(contents)) }
  }

  /** Match a single uninterrupted PlainText SubstitutionElement
   *  excluding special characters designated for parameter lists, delimiters and commands
   */
  def plainArgumentText : Parser[PlainText] = {
    (rep1 ( withoutAny(sigil, singleSpace, lineFeed, tabChar,
            quote(openParam, closeParam, delimParam)) | safeSigil )) ^^
      {  case(contents) => PlainText(listToString(contents)) }
  }

  /** Match a parameter list comprising any number of elements */
  def argumentList = {
    openParam ~> ( repsep((argumentElement *), delimParam) ) <~ closeParam ^^
      { case(blocks) => ElementsList(blocks.map(ElementBlock)) }
  }

  /** Match an argument list comprising any number of elements */
  def contentsList = {
    openList ~> ( repsep((listElement *), delimList) ) <~ closeList ^^
      { case(blocks) => ElementsList(blocks.map(ElementBlock)) }
  }

  /** Match a single Replacement SubstitutionElement with one Identifier */
  def replacement : Parser[Replacement] = {
    (sigil ~ openBrace ~ space ~> identifier <~ space <~ closeBrace) ^^ Replacement
  }

  /** Match a single Command SubstitutionElement with an argument list */
  def command : Parser[Command] = {
    sigil ~ openBrace ~ space ~> identifier ~ space ~ contentsList <~ space ~ closeBrace ^^
      { case (id ~ s ~ list) => Command(id, list) }
  }

  /** Match a command or replacement identifier */
  def identifier : Parser[Identifier] =
    ident ^^ { case(name) => Identifier(name.toLowerCase) }

  /** Match a single ParameterizedCommand SubstitutionElement with a
   *  parameter list and an argument list
   */
  def paramCommand : Parser[ParameterizedCommand] = {
    ((sigil ~ openBrace ~ space ~> identifier ~ space ~ argumentList ~
        space ~ opt(contentsList) <~ space ~ closeBrace)) ^^
      {
        case (id ~ s1 ~ params ~ s2 ~ Some(list)) =>
          ParameterizedCommand(id, params, list)
        case (id ~ s1 ~ params ~ s2 ~ None) =>
          ParameterizedCommand(id, params, ElementsList(Seq[ElementBlock]()))
      }
  }

  /** Match a sequence of characters allowable within an escape block */
  def escapeText : Parser[PlainText] =
    rep1 (withoutAny(openAngle, closeAngle)) ^^
      { case(contents) => PlainText(listToString(contents)) }

  /** Match an arbitrary set of paired angle brackets within an escape block */
  def inertAngleBlock : Parser[InertAngleBlock] =
    (openAngle ~> (anyEscapeElement *) <~ closeAngle) ^^ InertAngleBlock

  /** Match a top-level escape block (to be unescaped on final substitution */
  def escapeBlock : Parser[EscapeBlock] =
    (sigil ~ openAngle ~> (anyEscapeElement *) <~ closeAngle) ^^ EscapeBlock
}