package com.purloux.scala.substitutions.test.commands.manipulations
import com.purloux.scala.substitutions.test.utility.DefaultSubstitutor._
import org.scalatest.FlatSpec

class UpperSpec extends FlatSpec {
  
  "An Upper manipulation" should "replace any arguments with uppercase arguments" in {
    val input = "@{upper[ONE Two 123]}"
    val result = substitutor.sub(input)
    assert(result === "ONE TWO 123")
  }

  it should "replace multiple arguments with space-delimited uppercase arguments" in {
    val input = "@{upper[ONE|Two|123]}"
    val result = substitutor.sub(input)
    assert(result === "ONE TWO 123")
  }
}