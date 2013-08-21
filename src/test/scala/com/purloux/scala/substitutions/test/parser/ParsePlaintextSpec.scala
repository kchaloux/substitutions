package com.purloux.scala.substitutions.test.parser
import com.purloux.scala.substitutions.SubstitutionParser._
import com.purloux.scala.substitutions.SubstitutionElements._
import com.purloux.scala.substitutions.test.utility.ParserMatchers._
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.MatchResult

class ParsePlaintextSpec extends FlatSpec with ShouldMatchers {

  val matchPlainText = MatchParser[PlainText]("PlainText", plainText, {
    (pt : PlainText, input : String) => pt.contents == input 
  })
  
  "A Plaintext element" should "match any sequence of non-special characters" in {
    val input = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ[]{}()<>1234567890!@#$%^&*,./;'\\?|"
    input should (matchPlainText)
  }

  it should "not match sequences containing &lt;" in {
    "abc &lt; def" should not (matchPlainText)
  }

  it should "not match sequences containing &gt;" in {
    "abc &gt; def" should not (matchPlainText)
  }

  it should "not match sequences containing &amp;" in {
    "abc &amp; def" should not (matchPlainText)
  }

  it should "not match sequences containing &quot;" in {
    "abc &quot; def" should not (matchPlainText)
  }

  it should "not match sequences containing &apos;" in {
    "abc &apos; def" should not (matchPlainText)
  }

  it should "not match sequences containing any @{...} block" in {
    "abc @{replace} def" should not (matchPlainText)
  }

  it should "not match sequences containing any @<...> block" in {
    "abc @<escape> def" should not (matchPlainText)
  }
}