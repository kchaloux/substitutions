package com.purloux.scala.substitutions.test
import com.purloux.scala.substitutions.SubstitutionParser
import com.purloux.scala.substitutions.Substitutor
import com.purloux.scala.substitutions.ImplicitSubstitutions._
import org.scalatest.FlatSpec

class ImplicitSubstitutionsSpec extends FlatSpec {
  "A Sub call" should "perform substitutions using implicit substitutor" in {
    val result = "@{caps[capitalize]}" sub ()
    assert(result === "Capitalize")
  }

  it should "recognize provided arguments" in {
    val result = "@{caps[@{name}]}" sub Map("name" -> "john")
    assert(result === "John")
  }

  "A SubMulti call" should "perform multiline substitutions using implicit substitutor" in {
    val template = 
    """
      @{caps[one]}
      @{caps[two]}
      @{caps[three]}
    """

    val result = template subMulti ()
    assert(result === "One Two Three")
  }

  it should "recognize provided arguments" in {
    val template =
    """
      @{caps[@{1}]}
      @{caps[@{2}]}
      @{caps[@{3}]}
    """

    val result = template subMulti Map(
      "1" -> "one",
      "2" -> "two",
      "3" -> "three")
    assert(result === "One Two Three")
  }

  "The Implicit Substitutor" should "perform substitutions for only the default commands" in {
    val template = "@{title[one two three]}"
    val result = template sub ()
    assert(result === "@{title[one two three]}")
  }

  it should "be overridden by local implicit substitutors" in {
    val template = "@{title[one two three]}"
    implicit val sub = new Substitutor().withCommand("title", { 
      _.map {
        _.split(" ").map {
          _.toLowerCase.capitalize
        }
        .mkString(" ")
      }
      .mkString(" ")
    })

    val result = template sub ()
    assert(result === "One Two Three")
  }
}