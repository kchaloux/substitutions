package com.purloux.scala.substitutions.test
import com.purloux.scala.substitutions.test.DefaultSubstitutor._
import org.scalatest.FlatSpec

class EscapedSpec extends FlatSpec {

  "An Escaped element" should "return its inner contents without other substitutions" in {
    val input = "@<@{rand}>"
    val result = substitutor.sub(input)
    assert(result === "@{rand}")
  }

  it should "allow the user to quote commas within parameter lists" in {
    val input = "@{join (@<,>) [one|two|three]}"
    val result = substitutor.sub(input)
    assert(result === "one,two,three")
  }

  it should "allow the user to quote vertical bars within argument lists" in {
    val input = "@{dup (10) [@<|>]}"
    val result = substitutor.sub(input)
    assert(result === "||||||||||")
  }
}