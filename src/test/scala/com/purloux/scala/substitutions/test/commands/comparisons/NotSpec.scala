package com.purloux.scala.substitutions.test.commands.comparisons
import com.purloux.scala.substitutions.Substitutor
import org.scalatest.FlatSpec

class NotSpec extends FlatSpec {
  val substitutor = new Substitutor().withRandomSeed(0)

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