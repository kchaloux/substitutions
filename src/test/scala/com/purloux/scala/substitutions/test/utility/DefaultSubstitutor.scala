package com.purloux.scala.substitutions.test.utility
import com.purloux.scala.substitutions.Substitutor

object DefaultSubstitutor {
  val substitutor = new Substitutor().withRandomSeed(0)
}