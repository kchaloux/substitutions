package com.purloux.scala.substitutions.test
import com.purloux.scala.substitutions.test.utility.DefaultSubstitutor._
import org.scalatest.FlatSpec

class ComparisonNot extends FlatSpec {

  "A Not inversion" should "yield 'true' when provided 'false'" in {
    val input = "@{not(false)}"
    val result = substitutor.sub(input)
    assert(result === "true")
  }

  it should "yield 'false' when provided 'true'" in {
    val input = "@{not(true)}"
    val result = substitutor.sub(input)
    assert(result === "false")
  }

  it should "yield the opposite of any given comparison" in {
    val input = "@{not(eq, 10, 10)}"
    val result = substitutor.sub(input)
    assert(result === "false")
  }
}