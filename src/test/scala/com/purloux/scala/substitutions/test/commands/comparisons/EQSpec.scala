package com.purloux.scala.substitutions.test.commands.comparisons
import com.purloux.scala.substitutions.Substitutor
import org.scalatest.FlatSpec

class EQSpec extends FlatSpec {
  val substitutor = new Substitutor().withRandomSeed(0)
  
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