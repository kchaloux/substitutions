package com.purloux.scala.substitutions.test.parser
import com.purloux.scala.substitutions.parsers.SubstitutionParser
import com.purloux.scala.substitutions.SubstitutionElements._
import com.purloux.scala.substitutions.test.utility.ParserMatcher
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class ParseIdentifierSpec extends FlatSpec with ShouldMatchers {
  val matcher = ParserMatcher(SubstitutionParser)
  val matchIdentifier = matcher.MatchValidatedParser("Identifier")( _.identifier )
    { (id, input) => id.name == input.toLowerCase }

  "An Identifier" should "match any sequence of alphanumeric characters" in {
    "0123456789aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ" should matchIdentifier
  }

  it should "match sequences containing underscores" in {
    "_ident_i_fier_" should matchIdentifier
  }

  it should "match sequences containing dashes" in {
    "-ident-i-fier-" should matchIdentifier
  }

  it should "not match sequences containing spaces" in {
    "ident i fier" should not (matchIdentifier)
  }

  it should "not distinguish between upper and lower case identifiers" in {
    val input1 = "identifier"
    val input2 = "IDENTIFIER"

    val ident1 = SubstitutionParser.parseAll(SubstitutionParser.identifier, input1) match {
      case SubstitutionParser.Success(rp, _) => rp
      case _              => fail(s"Unable to parse `$input1`")
    }

    val ident2 = SubstitutionParser.parseAll(SubstitutionParser.identifier, input2) match {
      case SubstitutionParser.Success(rp, _) => rp
      case _              => fail(s"Unable to parse `$input2`")
    }

    assert(ident1 === ident2)
  }
}