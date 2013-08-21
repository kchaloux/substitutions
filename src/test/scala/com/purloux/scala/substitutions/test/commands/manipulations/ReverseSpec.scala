package com.purloux.scala.substitutions.test.commands.manipulations
import com.purloux.scala.substitutions.test.utility.DefaultSubstitutor._
import org.scalatest.FlatSpec

class ReverseSpec extends FlatSpec {
  
  "A Reverse manipulation" should "reverse the contents of an argument" in {
    val input = "@{reverse[dracula]}"
    val result = substitutor.sub(input)
    assert(result === "alucard")
  }

  it should "reverse the contents (but not orders) of multiple arguments, space delimited" in {
    val input = "@{reverse[dracula|one|two|three]}"
    val result = substitutor.sub(input)
    assert(result === "alucard eno owt eerht")
  }
}