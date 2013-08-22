package com.purloux.scala.substitutions.test.substitutor
import com.purloux.scala.substitutions.Substitutor
import org.scalatest.FlatSpec

class SubstitutorPlaintextTests extends FlatSpec {
  val substitutor = new Substitutor().withRandomSeed(0)
  
  "A Substitutor plaintext substitution" should "perform no changes to the input text" in {
    val input = "A plaintext sentence ~|{@}|~"
    val result = substitutor.sub(input)
    assert(result === input)
  }
}