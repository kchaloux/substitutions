package com.purloux.scala.substitutions.test
import com.purloux.scala.substitutions.test.utility.DefaultSubstitutor._
import org.scalatest.FlatSpec

class ComparisonGTE extends FlatSpec {

  "A GTE comparison" should "yield 'true' for a first argument greater-or-equal to the second" in {
    val input = "@{gte(0,0)}"
    val result = substitutor.sub(input)
    assert(result === "true")
  }

  it should "yield 'false' for a first argument not greater-or-equal to the second" in {
    val input = "@{gte(0,1)}"
    val result = substitutor.sub(input)
    assert(result === "false")
  }
}