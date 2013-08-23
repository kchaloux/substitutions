package com.purloux.scala.substitutions.test.parser
import com.purloux.scala.substitutions.SubstitutionParser
import com.purloux.scala.substitutions.SubstitutionElements._
import com.purloux.scala.substitutions.test.utility.ParserMatcher
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class ParseEscapeBlockSpec extends FlatSpec with ShouldMatchers {
  val matcher = ParserMatcher(SubstitutionParser)
  val matchEscapeBlock = matcher.MatchParser("EscapeBlock")( _.escapeBlock )

  "An escape block" should "match any element following the for @<escaped>" in {
    "@<escaped>" should matchEscapeBlock
  }

  it should "match @<nested <angle brackets>> within an escape block" in {
    "@<escaped <block>>" should matchEscapeBlock
  }

  it should "match @<nested @<escapes>> with an escape block" in {
    "@<escaped @<inner>>" should matchEscapeBlock
  }

  it should "not match blocks with a @ <space> between the sigil and angle bracket" in {
    "@ <not escaped>" should not (matchEscapeBlock)
  }
}