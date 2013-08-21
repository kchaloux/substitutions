package com.purloux.scala.substitutions.test
import com.purloux.scala.substitutions.SubstitutionParser
import com.purloux.scala.substitutions.Substitutor
import org.scalatest.FlatSpec

class SubstitutorSpec extends FlatSpec {
  private val substitutor = new Substitutor().withRandomSeed(0)

  "A Plaintext element" should "yield itself" in {
    val input = "A plaintext sentence ~|{@}|~"
    val result = substitutor.sub(input)
    assert(result === input)
  }

  "A Replacement element" should "replace its contents with a map value" in {
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

  "A Multiline Substitution" should "format multiline strings without newlines" in {
    val template =
    """
      @{1}
      @{2}
      @{3}
    """

    val result = substitutor.subMulti(template, Map(
      "1" -> "one",
      "2" -> "two",
      "3" -> "three"))
    val expected = "one two three"

    assert(result === expected)
  }

  it should "successfully replace non-trivial templates" in {
    val template =
    """
      @{caps[@{pro_sub}]} steps into the @{rand
        [gloomy_
        |murky_
        |dank]}
      dungeon,
      drawing @{pro_pos} @{weapon} at the ready.
      @{br}_
      Suddenly, @{pro_sub} is ambushed by
      @{plural  (@{monster_number})
        [a lone @{monster}
        |a @{rand
            [troupe_
            |gaggle_
            |band]}
          of @{monster}s!]}
    """

    val result = substitutor.subMulti(template, Map(
      "pro_sub" -> "she",
      "pro_pos" -> "her",
      "weapon" -> "bow",
      "monster_number" -> 5,
      "monster" -> "goblin"))

    val expected = "She steps into the gloomy dungeon, drawing her bow at the ready. \nSuddenly, she is ambushed by a gaggle of goblins!"
    assert(result === expected)
  }

  "A Substitutor" should "successfully register new commands at runtime" in {
    val subber = new Substitutor().withRandomSeed(0).withCommand("titlecase", { 
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
}