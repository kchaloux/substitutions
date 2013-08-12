package com.purloux.scala.substitutions.test
import com.purloux.scala.substitutions.test.DefaultSubstitutor._
import org.scalatest.FlatSpec

class CommandSelect extends FlatSpec {
  
  "A Select command" should "select the first branch corresponding to a true matching argument" in {
    val input = "@{select(lte, 100, 0, 50, 100, 200)[A|B|C|D]}"
    val result = substitutor.sub(input)
    assert(result === "C")
  }
}