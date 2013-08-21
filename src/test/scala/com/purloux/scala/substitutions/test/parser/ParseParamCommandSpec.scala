package com.purloux.scala.substitutions.test.parser
import com.purloux.scala.substitutions.SubstitutionParser._
import com.purloux.scala.substitutions.SubstitutionElements._
import com.purloux.scala.substitutions.test.utility.ParserMatchers._
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.MatchResult

class ParseParamCommandSpec extends FlatSpec with ShouldMatchers {

  val matchParamCommand = MatchParser[ParameterizedCommand]("ParamCommand", paramCommand)

  "A Parameterized Command element" should "match any block of the form @{id (...) [...]}" in {
    "@{pcommand (...) [...]}" should matchParamCommand
  }

  it should "match blocks omitting a contents list" in {
    "@{pcommand (...)}" should matchParamCommand
  }

  it should "match blocks with arbitrary spaces around its identifier" in {
    "@{   command  () []}" should matchParamCommand
  }

  it should "match blocks with no spaces between its identifier and inputs" in {
    "@{command()[]}" should matchParamCommand
  }

  it should "match blocks including @ as a parameter argument" in {
    "@{command (@) [...]}" should matchParamCommand
  }

  it should "match blocks including @ as a contents argument" in {
    "@{command (...) [@]}" should matchParamCommand
  }

  it should "not match blocks omitting a parameter list" in {
    "@{command[]}" should not (matchParamCommand)
  }

  it should "not match blocks omitting a parameter list and contents list" in {
    "@{command}" should not (matchParamCommand)
  }
}