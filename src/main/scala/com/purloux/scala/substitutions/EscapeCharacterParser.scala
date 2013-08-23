package com.purloux.scala.substitutions
import scala.util.parsing.combinator.RegexParsers
import scala.language.postfixOps

/** Post-processor parser for special escape characters */
object EscapeCharacterParser extends RegexParsers with ParserPatterns {
  import SubstitutionElements._
  import util.matching.Regex
  override val skipWhitespace = false

  // Basic syntax elements and Regular Expressions
  private val sigil       = "&"
  private val terminator  = ";"
  private val alphaSeq    = "[a-zA-Z]+".r

  private def anyElement  = ( escapeCharacter | plainText )
  private def safeSigil   = sigil <~ not(alphaSeq ~ terminator)

  /** Match a body of text consisting of a block of Escapes and Plaintext */
  def wholeText : Parser[ElementBlock] =
    start ~> (anyElement *) <~ end ^^ { ElementBlock }

  /** Match a single uninterrupted PlainText SubstitutionElement */
  def plainText : Parser[PlainText] =
    rep1 (withoutAny(sigil) | safeSigil ) ^^
      { case (contents) => PlainText(listToString(contents)) }

  /** Match a single sigil-escaped character sequence */
  def escapeCharacter : Parser[EscapeCharacter] =
    sigil ~> alphaSeq <~ terminator ^^ EscapeCharacter
}