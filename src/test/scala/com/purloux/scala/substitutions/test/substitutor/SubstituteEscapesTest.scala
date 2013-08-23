package com.purloux.scala.substitutions.test.substitutor
import com.purloux.scala.substitutions.Substitutor
import org.scalatest.FlatSpec

class SubstituteEscapesTests extends FlatSpec {
  val substitutor = new Substitutor().withRandomSeed(0)
  
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

  it should "allow nesting of escaped elements" in {
    val input = "@<@<escaped element>>"
    val result = substitutor.sub(input)
    assert(result === "@<escaped element>")
  }

  it should "allow arbitrary mixes of plaintext and nested escaped elements" in {
    val input = "@<one @<two> three @<four @<five> six>>"
    val result = substitutor.sub(input)
    assert(result === "one @<two> three @<four @<five> six>")
  }

  it should "allow arbitrary <> pairs (when matched)" in {
    val input = "@<escape <one> <two> <three>>"
    val result = substitutor.sub(input)
    assert(result === "escape <one> <two> <three>")
  }

  "An ampersand escape" should "replace &lt; with <" in
    assert(substitutor.sub("&lt;") === "<")

  it should "replace &less; with <" in
    assert(substitutor.sub("&less;") === "<")

  it should "replace &gt; with >" in
    assert(substitutor.sub("&gt;") === ">")

  it should "replace &greater; with >" in
    assert(substitutor.sub("&greater;") === ">")

  it should "replace &br; with a newline" in
    assert(substitutor.sub("&br;") === "\n") 

  it should "replace &break; with a newline" in
    assert(substitutor.sub("&break;") === "\n")

  it should "replace $at; with @" in
    assert(substitutor.sub("&at;") === "@")

  it should "replace &sigil; with @" in
    assert(substitutor.sub("&sigil;") === "@")

  it should "reproduce itself if not given any the above contents" in
    assert(substitutor.sub("&fail;") === "&fail;")

  it should "replace itself from within the middle of plain text" in
    assert(substitutor.sub("one &lt; two") === "one < two")

  it should "replace itself when provided inside an escape block" in
    assert(substitutor.sub("@<&lt;>") === "<")

  it should "replace itself when provided inside a nested escape block" in
    assert(substitutor.sub("@<@<&lt;>>") === "@<<>")
}