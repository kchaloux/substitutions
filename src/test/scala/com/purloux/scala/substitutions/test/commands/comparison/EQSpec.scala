package com.purloux.scala.substitutions.test
import com.purloux.scala.substitutions.test.utility.DefaultSubstitutor._
import org.scalatest.FlatSpec

class EQSpec extends FlatSpec {
  
  "An EQ comparison" should "yield 'true' for any values with string equality" in {
    val input = "@{eq(a, a)}"
    val result = substitutor.sub(input)
    assert(result === "true")
  }

  it should "yield 'false' for any values without string equality" in {
    val input = "@{eq(a, b)}"
    val result = substitutor.sub(input)
    assert(result === "false")
  }
}