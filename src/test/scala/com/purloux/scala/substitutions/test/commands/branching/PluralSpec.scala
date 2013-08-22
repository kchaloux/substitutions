package com.purloux.scala.substitutions.test.commands.branching
import com.purloux.scala.substitutions.Substitutor
import org.scalatest.FlatSpec

class PluralSpec extends FlatSpec {
  val substitutor = new Substitutor().withRandomSeed(0)
  
  "A Plural branch" should "select the leftmost branch for numeric inputs of 1" in {
    val input = "@{plural (1) [apple|apples]}"
    val result = substitutor.sub(input)
    assert(result === "apple")
  }

  it should "select the rightmost branch for any other numeric inputs" in {
    val input = "@{plural (2) [apple|apples]}"
    val result = substitutor.sub(input)
    assert(result === "apples")
  }
}