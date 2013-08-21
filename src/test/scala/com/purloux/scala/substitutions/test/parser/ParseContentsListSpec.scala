package com.purloux.scala.substitutions.test.parser
import com.purloux.scala.substitutions.SubstitutionParser._
import com.purloux.scala.substitutions.SubstitutionElements._
import com.purloux.scala.substitutions.test.utility.ParserMatchers._
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.MatchResult

class ParseContentsListSpec extends FlatSpec with ShouldMatchers {
  
  val matchElements = MatchListElements("ContentsList", contentsList)
  val matchContentsList = MatchParser[ElementsList]("ContentsList", contentsList)

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