package com.purloux.scala.substitutions.test
import com.purloux.scala.substitutions.test.utility.DefaultSubstitutor._
import org.scalatest.FlatSpec

class ManipulationJoin extends FlatSpec {

  "A Join manipulation" should "intercalate a list of arguments with a separator" in {
    val input = "@{join (-) [a|b|c]}"
    val result = substitutor.sub(input)
    assert(result === "a-b-c")
  }

  it should "ignore empty arguments in the final string" in {
    val input = "@{join (-) [a|b|c||d||e]}"
    val result = substitutor.sub(input)
    assert(result === "a-b-c-d-e")
  }
}