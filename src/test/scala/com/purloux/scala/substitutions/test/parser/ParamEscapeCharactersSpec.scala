package com.purloux.scala.substitutions.test.parser
import com.purloux.scala.substitutions.SubstitutionParser._
import com.purloux.scala.substitutions.SubstitutionElements._
import com.purloux.scala.substitutions.test.utility.ParserMatchers._
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.MatchResult

class ParseEscapeCharactersSpec extends FlatSpec with ShouldMatchers {

  val matchEscape = MatchParser[EscapeCharacter]("EscapeCharacter", escapeCharacter)

  "An XML-Style escape" should "match &lt;" in { "&lt;" should matchEscape }
  it should "match &gt;" in { "&gt;" should matchEscape }
  it should "match &amp;" in { "&amp;" should matchEscape }
  it should "match &quot;" in { "&quot;" should matchEscape }
  it should "match &apos;" in { "&apos;" should matchEscape }
}