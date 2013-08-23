package com.purloux.scala.substitutions.test.parser
import com.purloux.scala.substitutions.SubstitutionParser
import com.purloux.scala.substitutions.SubstitutionElements._
import com.purloux.scala.substitutions.test.utility.ParserMatcher
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class ParseReplacementSpec extends FlatSpec with ShouldMatchers {
  val matcher = ParserMatcher(SubstitutionParser)
  val matchReplacement = matcher.MatchValidatedParser("Replacement")( _.replacement )
    { (rp, input) => rp.ident.name == input.substring(2, input.length - 1).trim }

  "A Replacement element" should "match a sequence following the form @{identifier}" in {
    "@{replacement}" should matchReplacement
  }

  it should "ignore arbitrary spaces between the identifier and braces" in {
    "@{  replacement  }" should matchReplacement
  }

  it should "allow underscores within identifiers" in {
    "@{replace_ment}" should matchReplacement
  }

  it should "allow dashes within identifiers" in {
    "@{replace-ment}" should matchReplacement
  }

  it should "not allow parentheses at any point" in {
    "@{replacement()}" should not (matchReplacement)
  }

  it should "not allow square brackets at any point" in {
    "@{replacement[]}" should not (matchReplacement)
  }

  it should "not distinguish between upper and lower case identifiers" in {
    val input1 = "@{replacement}"
    val input2 = "@{REPLACEMENT}"

    val replace1 = SubstitutionParser.parseAll(SubstitutionParser.replacement, input1) match {
      case SubstitutionParser.Success(rp, _) => rp
      case _              => fail(s"Unable to parse `$input1`")
    }

    val replace2 = SubstitutionParser.parseAll(SubstitutionParser.replacement, input2) match {
      case SubstitutionParser.Success(rp, _) => rp
      case _              => fail(s"Unable to parse `$input2`")
    }

    assert(replace1 === replace2)
  }
}