package com.purloux.scala.substitutions.test.parser
import com.purloux.scala.substitutions.parsers.SubstitutionParser
import com.purloux.scala.substitutions.SubstitutionElements._
import com.purloux.scala.substitutions.test.utility.ParserMatcher
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class WhiteSpaceSpec extends FlatSpec with ShouldMatchers {
  val matcher = ParserMatcher(SubstitutionParser)
  val matchWhiteSpace = matcher.MatchValidatedParser("WhiteSpace")( _.whiteSpaceText )
    { (pt, input) => pt.contents == input }

  "A WhiteSpace element" should "match a single space" in {
    " " should matchWhiteSpace
  }

  it should "match multiple spaces" in {
    "     " should matchWhiteSpace
  }

  it should "match a single newline" in {
    "\n" should matchWhiteSpace
  }

  it should "match a newline preceeded by a carriage return" in {
    "\r\n" should matchWhiteSpace
  }

  it should "match multiple newlines" in {
    "\n\n\n" should matchWhiteSpace
  }

  it should "match a single tab character" in {
    "\t" should matchWhiteSpace
  }

  it should "match multiple tab characters" in {
    "\t\t\t" should matchWhiteSpace
  }

  it should "match a mix of spaces, newlines and tab characters" in {
    "  \n\t  " should matchWhiteSpace
  }

  it should "not match any mix of whitespace characters with non-whitespace characters" in {
    "one\ntwo\nthree\t" should not (matchWhiteSpace)
  }
}