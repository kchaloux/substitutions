package com.purloux.scala.substitutions.test
import com.purloux.scala.substitutions.Substitutor

object DefaultSubstitutor {
  val substitutor = new Substitutor().withRandomSeed(0)
}