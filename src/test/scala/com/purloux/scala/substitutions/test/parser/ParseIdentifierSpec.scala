package com.purloux.scala.substitutions.test.parser
import com.purloux.scala.substitutions.SubstitutionParser._
import com.purloux.scala.substitutions.SubstitutionElements._
import com.purloux.scala.substitutions.test.utility.ParserMatchers._
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.MatchResult

class ParseIdentifierSpec extends FlatSpec with ShouldMatchers {
  
  val matchIdentifier = MatchParser[Identifier]("Identifier", identifier, {
    (id : Identifier, input : String) => id.name == input.toLowerCase
  })

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

    val ident1 = parseAll(identifier, input1) match {
      case Success(rp, _) => rp
      case _              => fail(s"Unable to parse `$input1`")
    }

    val ident2 = parseAll(identifier, input2) match {
      case Success(rp, _) => rp
      case _              => fail(s"Unable to parse `$input2`")
    }

    assert(ident1 === ident2)
  }
}