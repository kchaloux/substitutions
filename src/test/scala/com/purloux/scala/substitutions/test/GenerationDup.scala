package com.purloux.scala.substitutions.test
import com.purloux.scala.substitutions.test.utility.DefaultSubstitutor._
import org.scalatest.FlatSpec

class GenerationDup extends FlatSpec {

  "A Dup manipulation" should "duplicate a single argument's contents the provided number of times" in {
    val input = "@{dup(10)[-]}"
    val result = substitutor.sub(input)
    assert(result === "----------")
  }

  it should "delimit each element in the result of a single argument with the first delimiter" in {
    val input = "@{dup(10,>)[-]}"
    val result = substitutor.sub(input)
    assert(result === "->->->->->->->->->-")
  }

  it should "delimit the results of multiple arguments with the second delimiter" in {
    val input = "@{dup(10,,@)[-|-]}"
    val result = substitutor.sub(input)
    assert(result === "----------@----------")
  }
}