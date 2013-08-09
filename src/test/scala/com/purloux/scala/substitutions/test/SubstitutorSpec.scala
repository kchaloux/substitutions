package com.purloux.scala.substitutions.test
import com.purloux.scala.substitutions.SubstitutionParser
import org.scalatest.FlatSpec
import com.purloux.scala.substitutions.Substitutor

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

  "A Rand command" should "replace its contents with one of the provided values" in {
    val input = "@{rand[1|2|3]}"
    val result = substitutor.sub(input)
    assert(result === "1")
  }

  "A Caps command" should "replace any arguments with capitalized arguments" in {
    val input = "@{caps[ONE Two 123]}"
    val result = substitutor.sub(input)
    assert(result === "One two 123")
  }

  it should "replace multiple arguments with space-delimited capitalized arguments" in {
    val input = "@{caps[ONE|Two|123]}"
    val result = substitutor.sub(input)
    assert(result === "One Two 123")
  }

  "An Upper command" should "replace any arguments with uppercase arguments" in {
    val input = "@{upper[ONE Two 123]}"
    val result = substitutor.sub(input)
    assert(result === "ONE TWO 123")
  }

  it should "replace multiple arguments with space-delimited uppercase arguments" in {
    val input = "@{upper[ONE|Two|123]}"
    val result = substitutor.sub(input)
    assert(result === "ONE TWO 123")
  }

  "A Lower command" should "replace any arguments with lowercase arguments" in {
    val input = "@{lower[ONE Two 123]}"
    val result = substitutor.sub(input)
    assert(result === "one two 123")
  }

  it should "replace multiple arguments with space-delimited lowercase arguments" in {
    val input = "@{lower[ONE|Two|123]}"
    val result = substitutor.sub(input)
    assert(result === "one two 123")
  }

  "A Reverse command" should "reverse the contents of an argument" in {
    val input = "@{reverse[dracula]}"
    val result = substitutor.sub(input)
    assert(result === "alucard")
  }

  it should "reverse the contents (but not orders) of multiple arguments, space delimited" in {
    val input = "@{reverse[dracula|one|two|three]}"
    val result = substitutor.sub(input)
    assert(result === "alucard eno owt eerht")
  }

  "A Pluralize command" should "select the leftmost branch for numeric inputs of 1" in {
    val input = "@{pluralize(1)[apple|apples]}"
    val result = substitutor.sub(input)
    assert(result === "apple")
  }

  it should "select the rightmost branch for any other numeric inputs" in {
    val input = "@{pluralize(2)[apple|apples]}"
    val result = substitutor.sub(input)
    assert(result === "apples")
  }

  "An If command" should "select the leftmost branch when provided 'true'" in {
    val input = "@{if(true)[true|false]}"
    val result = substitutor.sub(input)
    assert(result === "true")
  }

  it should "select the rightmost branch when provided 'false'" in {
    val input = "@{if(false)[true|false]}"
    val result = substitutor.sub(input)
    assert(result === "false")
  }

  it should "recognize calls to boolean functions in 3-parameter mode" in {
    val input = "@{if (gt, 10, 20) [greater|less]}"
    val result = substitutor.sub(input)
    assert(result === "less")
  }

  "A Select command" should "select the first branch corresponding to a true matching argument" in {
    val input = "@{select(lte, 100, 0, 50, 100, 200)[A|B|C|D]}"
    val result = substitutor.sub(input)
    assert(result === "C")
  }

  "An EQ comparison" should "yield 'true' for any values with string equality" in {
    val input = "@{eq(a, a)}"
    val result = substitutor.sub(input)
    assert(result === "true")
  }

  it should "yield 'false' for any values without string equality" in {
    val input = "@{eq(a, b)}"
    val result = substitutor.sub(input)
    assert(result === "false")
  }

  "An LT comparison" should "yield 'true' when the first argument is less than the second" in {
    val input = "@{lt(0, 1)}"
    val result = substitutor.sub(input)
    assert(result === "true")
  }

  it should "yield 'false' when the first argument is not less than the second" in {
    val input = "@{lt(0, 0)}"
    val result = substitutor.sub(input)
    assert(result === "false")
  }

  "An LTE comparison" should "yield 'true' for a first argument lesser-or-equal to the second" in {
    val input = "@{lte(0,0)}"
    val result = substitutor.sub(input)
    assert(result === "true")
  }

  it should "yield 'false' for a first argument not lesser-or-equal to the second" in {
    val input = "@{lte(0,-1)}"
    val result = substitutor.sub(input)
    assert(result === "false")
  }

  "A GT comparison" should "yield 'true' for a first argument greater than the second" in {
    val input = "@{gt(1,0)}"
    val result = substitutor.sub(input)
    assert(result === "true")
  }

  it should "yield 'false' for a first argument not greater than the second" in {
    val input = "@{gt(0,0)}"
    val result = substitutor.sub(input)
    assert(result === "false")
  }

  "A GTE comparison" should "yield 'true' for a first argument greater-or-equal to the second" in {
    val input = "@{gte(0,0)}"
    val result = substitutor.sub(input)
    assert(result === "true")
  }

  it should "yield 'false' for a first argument not greater-or-equal to the second" in {
    val input = "@{gte(0,1)}"
    val result = substitutor.sub(input)
    assert(result === "false")
  }

  "A Dup command" should "duplicate a single argument's contents the provided number of times" in {
    val input = "@{dup(10)[-]}"
    val result = substitutor.sub(input)
    assert(result === "----------")
  }

  it should "delimit each element in the result of a single argument with the first delimiter" in {
    val input = "@{dup(10,>)[-]}"
    val result = substitutor.sub(input)
    assert(result === "->->->->->->->->->-")
  }

  it should "delimit the results of multiple arguments with the second delimiter" in {
    val input = "@{dup(10,,@)[-|-]}"
    val result = substitutor.sub(input)
    assert(result === "----------@----------")
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
      @{pluralize (@{monster_number})
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

    val expected = "She steps into the murky dungeon, drawing her bow at the ready. \nSuddenly, she is ambushed by a gaggle of goblins!"
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