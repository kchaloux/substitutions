package com.purloux.scala.substitutions.test.parser
import com.purloux.scala.substitutions.EscapeCharacterParser
import com.purloux.scala.substitutions.SubstitutionElements._
import com.purloux.scala.substitutions.test.utility.ParserMatcher
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class ParseEscapeCharactersSpec extends FlatSpec with ShouldMatchers {
  val matcher = ParserMatcher(EscapeCharacterParser)
  val matchEscape = matcher.MatchParser("EscapeCharacter")( _.escapeCharacter )

  "An XML-Style escape" should "match &lt;" in { "&lt;" should matchEscape }
  it should "match &gt;" in { "&gt;" should matchEscape }
  it should "match &amp;" in { "&amp;" should matchEscape }
  it should "match &quot;" in { "&quot;" should matchEscape }
  it should "match &apos;" in { "&apos;" should matchEscape }
}