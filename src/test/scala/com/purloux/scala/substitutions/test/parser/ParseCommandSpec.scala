package com.purloux.scala.substitutions.test.parser
import com.purloux.scala.substitutions.SubstitutionParser
import com.purloux.scala.substitutions.SubstitutionElements._
import com.purloux.scala.substitutions.test.utility.ParserMatcher
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class ParseCommandSpec extends FlatSpec with ShouldMatchers {
  val matcher = ParserMatcher(SubstitutionParser)
  val matchCommand = matcher.MatchParser("Command")( _.command )

  "A Command element" should "match any block of the form @{id [...]}" in {
    "@{command [...]}" should matchCommand
  }

  it should "match blocks with an empty contents list []" in {
    "@{command []}" should matchCommand
  }

  it should "match blocks with arbitrary spaces around its identifier" in {
    "@{   command   []}" should matchCommand
  }

  it should "match blocks with no spaces between its identifier and contents list" in {
    "@{command[]}" should matchCommand
  }

  it should "match blocks including @ as a contents argument" in {
    "@{command [@]}" should matchCommand
  }

  it should "not match blocks containing a parameter list @{id (...) [...]}" in {
    "@{command (...) [...]}" should not (matchCommand)
  }

  it should "not match blocks omitting a contents list" in {
    "@{command  }" should not (matchCommand)
  }
}