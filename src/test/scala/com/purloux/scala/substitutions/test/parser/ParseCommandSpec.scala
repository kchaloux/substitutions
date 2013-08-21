package com.purloux.scala.substitutions.test.parser
import com.purloux.scala.substitutions.SubstitutionParser._
import com.purloux.scala.substitutions.SubstitutionElements._
import com.purloux.scala.substitutions.test.utility.ParserMatchers._
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.MatchResult

class ParseCommandSpec extends FlatSpec with ShouldMatchers {

  val matchCommand = MatchParser[Command]("Command", command)

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