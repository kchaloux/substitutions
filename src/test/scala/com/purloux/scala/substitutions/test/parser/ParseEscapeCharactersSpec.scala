package com.purloux.scala.substitutions.test.parser
import com.purloux.scala.substitutions.parsers.EscapeCharacterParser
import com.purloux.scala.substitutions.SubstitutionElements._
import com.purloux.scala.substitutions.test.utility.ParserMatcher
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class ParseEscapeCharactersSpec extends FlatSpec with ShouldMatchers {
  val matcher = ParserMatcher(EscapeCharacterParser)
  val matchEscape = matcher.MatchParser("EscapeCharacter")( _.escapeCharacter )

  "An XML-Style escape" should "match @lt;" in { "@lt;" should matchEscape }
  it should "match @at;" in { "@at;" should matchEscape }
  it should "match @br;" in { "@br;" should matchEscape }
  it should "match @gt;" in { "@gt;" should matchEscape }
  it should "match @less;" in { "@less;" should matchEscape }
  it should "match @sigil;" in {"@sigil;" should matchEscape }
  it should "match @break;" in { "@break;" should matchEscape }
  it should "match @greater;" in { "@greater;" should matchEscape }
}