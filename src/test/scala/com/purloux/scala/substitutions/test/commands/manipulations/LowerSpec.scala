package com.purloux.scala.substitutions.test.commands.manipulations
import com.purloux.scala.substitutions.Substitutor
import org.scalatest.FlatSpec

class LowerSpec extends FlatSpec {
  val substitutor = new Substitutor().withRandomSeed(0)
  
  "A Lower manipulation" should "replace any arguments with lowercase arguments" in {
    val input = "@{lower[ONE Two 123]}"
    val result = substitutor.sub(input)
    assert(result === "one two 123")
  }

  it should "replace multiple arguments with space-delimited lowercase arguments" in {
    val input = "@{lower[ONE|Two|123]}"
    val result = substitutor.sub(input)
    assert(result === "one two 123")
  }
}