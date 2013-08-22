package com.purloux.scala.substitutions.test.substitutor
import com.purloux.scala.substitutions.Substitutor
import org.scalatest.FlatSpec

class SubstitutorMultilineTests extends FlatSpec {
  val substitutor = new Substitutor().withRandomSeed(0)

  "A Substitutor multiline substitution" should "format multiline strings without newlines" in {
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
}