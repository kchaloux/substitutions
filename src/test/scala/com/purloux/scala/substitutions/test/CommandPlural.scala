package com.purloux.scala.substitutions.test
import com.purloux.scala.substitutions.test.DefaultSubstitutor._
import org.scalatest.FlatSpec

class CommandPlural extends FlatSpec {
  
  "A Plural command" should "select the leftmost branch for numeric inputs of 1" in {
    val input = "@{plural (1) [apple|apples]}"
    val result = substitutor.sub(input)
    assert(result === "apple")
  }

  it should "select the rightmost branch for any other numeric inputs" in {
    val input = "@{plural (2) [apple|apples]}"
    val result = substitutor.sub(input)
    assert(result === "apples")
  }
}