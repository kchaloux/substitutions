package com.purloux.scala.substitutions.test
import com.purloux.scala.substitutions.test.utility.DefaultSubstitutor._
import org.scalatest.FlatSpec

class ComparisonLTE extends FlatSpec {
  
  "An LTE comparison" should "yield 'true' for a first argument lesser-or-equal to the second" in {
    val input = "@{lte(0,0)}"
    val result = substitutor.sub(input)
    assert(result === "true")
  }

  it should "yield 'false' for a first argument not lesser-or-equal to the second" in {
    val input = "@{lte(0,-1)}"
    val result = substitutor.sub(input)
    assert(result === "false")
  }
}