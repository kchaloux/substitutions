package com.purloux.scala.substitutions.test.commands.branching
import com.purloux.scala.substitutions.Substitutor
import org.scalatest.FlatSpec

class RandSpec extends FlatSpec {
  val substitutor = new Substitutor().withRandomSeed(0)
  
  "A Rand branch" should "replace its contents with one of the provided values" in {
    val input = "@{rand[1|2|3]}"
    val result = substitutor.sub(input)
    assert(result === "1")
  }
}