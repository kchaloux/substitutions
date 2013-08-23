package com.purloux.scala.substitutions.test.parser
import com.purloux.scala.substitutions.parsers.SubstitutionParser
import com.purloux.scala.substitutions.SubstitutionElements._
import com.purloux.scala.substitutions.test.utility.ParserMatcher
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class ParseContentsListSpec extends FlatSpec with ShouldMatchers {
  val matcher = ParserMatcher(SubstitutionParser)
  val matchElements = matcher.MatchListElements("ContentsList")( _.contentsList )
  val matchContentsList = matcher.MatchParser("ContentsList")( _.contentsList )

  "A Contents List" should "match elements surrounded with [square brackets]" in {
    "[square brackets]" should matchContentsList
  }

  it should "find no elements in []" in {
    ("[]", Seq[String]()) should matchElements
  }

  it should "find one element in [abc]" in {
    ("[abc]", Seq("abc")) should matchElements
  } 

  it should "find two elements in [abc|def]" in {
    ("[abc|def]", Seq("abc", "def")) should matchElements
  }

  it should "find three elements in [abc|def|ghi]" in {
    ("[abc|def|ghi]", Seq("abc", "def", "ghi")) should matchElements
  }

  it should "find one element in [abc|]" in {
    ("[abc|]", Seq("abc")) should matchElements
  }

  it should "treat elements with replacements [one @{two} three] as a single element" in {
    ("[one @{two} three]", Seq("one @{two} three")) should matchElements
  }

  it should "treat elements with escapes [one @<two> three] as a single element" in {
    ("[one @<two> three]", Seq("one two three")) should matchElements
  }

  it should "treat elements with commands [one @{foo[]} three] as a single element" in {
    ("[one @{foo[]} three]", Seq("one @{foo[]} three")) should matchElements
  }

  it should "treat elements with paramCommands [one @{foo()[]} three] as a single element" in {
    ("[one @{foo()[]} three]", Seq("one @{foo()[]} three")) should matchElements
  }

  it should "not match unmatched, unescaped open-square-brackets [ within an element" in {
    "[abc[]" should not (matchContentsList)
  }

  it should "not match unmatched, unescaped close-square-brackets ] within an element" in {
    "[abc]]" should not (matchContentsList)
  }

  it should "match escaped open-square brackets @<[> within an element" in {
    ("[abc@<[>]", Seq("abc[")) should matchElements
  }

  it should "match escaped close-square-brackets @<]> within an element" in {
    ("[abc@<]>]", Seq("abc]")) should matchElements
  }
}