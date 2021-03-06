package com.purloux.scala.substitutions.test.commands.branching
import com.purloux.scala.substitutions.Substitutor
import org.scalatest.FlatSpec

class IfSpec extends FlatSpec {
  val substitutor = new Substitutor().withRandomSeed(0)
  
  "An If branch" should "select the leftmost branch when provided 'true'" in {
    val input = "@{if(true)[true|false]}"
    val result = substitutor.sub(input)
    assert(result === "true")
  }

  it should "select the rightmost branch when provided 'false'" in {
    val input = "@{if(false)[true|false]}"
    val result = substitutor.sub(input)
    assert(result === "false")
  }

  it should "recognize calls to boolean functions in 3-parameter mode" in {
    val input = "@{if (gt, 10, 20) [greater|less]}"
    val result = substitutor.sub(input)
    assert(result === "less")
  }
}