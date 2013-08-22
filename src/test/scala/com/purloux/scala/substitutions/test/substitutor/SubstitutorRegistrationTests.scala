package com.purloux.scala.substitutions.test.substitutor
import com.purloux.scala.substitutions.Substitutor
import org.scalatest.FlatSpec

class SubstitutorRegistrationTests extends FlatSpec {
  "A Substitutor" should "successfully register new commands at runtime" in {
    val subber = new Substitutor().withCommand("titlecase", { 
      _.map { 
        _.split(" ").map {
          _.toLowerCase.capitalize
        }
        .mkString(" ")
      }
      .mkString(" ")
    })

    val template = "@{titlecase[the tale of twelve tarrasques]}"
    val result = subber.sub(template)
    val expected = "The Tale Of Twelve Tarrasques"
    assert(result === expected)
  }

  it should "successfully register new parameterized commands at runtime" in {
    val subber = new Substitutor().withParamCommand("first", {
      (args : Seq[String]) => (contents : Seq[String]) => {
        val amount = args(0).toInt
        contents.map(_.take(amount)).mkString(" ")
      }
    })

    val template = "@{first (3) [subjective|objective|possessive]}"
    val result = subber.sub(template)
    val expected = "sub obj pos"
    assert(result === expected)
  }
}