package com.purloux.scala.substitutions.test.parser
import com.purloux.scala.substitutions.SubstitutionParser
import com.purloux.scala.substitutions.SubstitutionElements._
import com.purloux.scala.substitutions.test.utility.ParserMatcher
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class ParsePlaintextSpec extends FlatSpec with ShouldMatchers {
  val matcher = ParserMatcher(SubstitutionParser)
  val matchPlainText = matcher.MatchValidatedParser("PlainText")( _.plainText )
    { (pt, input) => pt.contents == input }

  "A Plaintext element" should "match any sequence of non-special characters" in {
    val input = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ[]{}()<>1234567890!@#$%^&*,./;'\\?|"
    input should (matchPlainText)
  }

  it should "not match sequences containing adjacent @{ characters" in {
    "abc @{ def" should not (matchPlainText)
  }

  it should "not match sequences containing adjacent @< characters" in {
    "abc @< def" should not (matchPlainText)
  }

  it should "match sequences containing non-adjacent @ and { characters" in {
    "@ and {" should matchPlainText
  }

  it should "match sequences containing non-adjacent @ and < characters" in {
    "@ and <" should matchPlainText
  }
}