package com.purloux.scala.substitutions.test
import com.purloux.scala.substitutions.test.DefaultSubstitutor._
import org.scalatest.FlatSpec

class CommandRand extends FlatSpec {
  
  "A Rand command" should "replace its contents with one of the provided values" in {
    val input = "@{rand[1|2|3]}"
    val result = substitutor.sub(input)
    assert(result === "1")
  }
}