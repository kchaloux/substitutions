package com.purloux.scala.substitutions.test.commands.manipulations
import com.purloux.scala.substitutions.Substitutor
import org.scalatest.FlatSpec

class CapsSpec extends FlatSpec {
  val substitutor = new Substitutor().withRandomSeed(0)
  
  "A Caps manipulation" should "replace any arguments with capitalized arguments" in {
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