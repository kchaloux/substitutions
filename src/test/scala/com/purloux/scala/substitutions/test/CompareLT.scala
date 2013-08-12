package com.purloux.scala.substitutions.test
import com.purloux.scala.substitutions.test.DefaultSubstitutor._
import org.scalatest.FlatSpec

class CompareLT extends FlatSpec {
  
  "An LT comparison" should "yield 'true' when the first argument is less than the second" in {
    val input = "@{lt(0, 1)}"
    val result = substitutor.sub(input)
    assert(result === "true")
  }

  it should "yield 'false' when the first argument is not less than the second" in {
    val input = "@{lt(0, 0)}"
    val result = substitutor.sub(input)
    assert(result === "false")
  }
}