package com.purloux.scala.substitutions.test
import com.purloux.scala.substitutions.test.utility.DefaultSubstitutor._
import org.scalatest.FlatSpec

class GTSpec extends FlatSpec {
  
  "A GT comparison" should "yield 'true' for a first argument greater than the second" in {
    val input = "@{gt(1,0)}"
    val result = substitutor.sub(input)
    assert(result === "true")
  }

  it should "yield 'false' for a first argument not greater than the second" in {
    val input = "@{gt(0,0)}"
    val result = substitutor.sub(input)
    assert(result === "false")
  }
}