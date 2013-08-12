package com.purloux.scala.substitutions.test
import com.purloux.scala.substitutions.test.DefaultSubstitutor._
import org.scalatest.FlatSpec

class CommandCaps extends FlatSpec {
  
  "A Caps command" should "replace any arguments with capitalized arguments" in {
    val input = "@{caps[ONE Two 123]}"
    val result = substitutor.sub(input)
    assert(result === "One two 123")
  }

  it should "replace multiple arguments with space-delimited capitalized arguments" in {
    val input = "@{caps[ONE|Two|123]}"
    val result = substitutor.sub(input)
    assert(result === "One Two 123")
  }
}