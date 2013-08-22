package com.purloux.scala.substitutions.test.substitutor
import com.purloux.scala.substitutions.test.utility.DefaultSubstitutor._
import com.purloux.scala.substitutions.Substitutor
import org.scalatest.FlatSpec

class SubstitutorReplacementTests extends FlatSpec {
  "A Substitutor replacement substitution" should "replace the input contents with a map value" in {
    val input = "@{replace_me}"
    val result = substitutor.sub(input, Map("replace_me" -> "replaced"))
    assert(result === "replaced")
  }

  it should "remain the same when no map value is provided" in {
    val input = "@{replace_me}"
    val result = substitutor.sub(input)
    assert(result === "@{replace_me}")
  }

  it should "replace a single instance with the same id multiple times" in {
    val input = "@{a} @{a} @{a}"
    val result = substitutor.sub(input, Map(
        "a" -> "one"))
    assert(result === "one one one")
  }

  it should "replace multiple different instances with the provided values" in {
    val input = "@{a} @{b} @{c}"
    val result = substitutor.sub(input, Map(
      "a" -> "one",
      "b" -> "two",
      "c" -> "three"))
    assert(result === "one two three")
  }

  it should "be case insensitive with regards to replacement identifiers" in {
    val input = "@{player_name} @{PLAYER_NAME}"
    val result = substitutor.sub(input, Map(
      "player_name" -> "Player"))
    assert(result === "Player Player")
  }

  it should "be case insensitive with regards to map arguments" in {
    val input = "@{player_name} @{PLAYER_NAME}"
    val result = substitutor.sub(input, Map(
      "Player_Name" -> "Player"))
    assert(result === "Player Player")
  }
}