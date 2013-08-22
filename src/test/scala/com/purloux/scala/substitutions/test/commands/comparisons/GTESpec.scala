package com.purloux.scala.substitutions.test.commands.comparisons
import com.purloux.scala.substitutions.Substitutor
import org.scalatest.FlatSpec

class GTESpec extends FlatSpec {
  val substitutor = new Substitutor().withRandomSeed(0)

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