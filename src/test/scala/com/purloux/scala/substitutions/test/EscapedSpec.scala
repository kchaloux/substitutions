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

  it should "replace &gt; with >" in
    assert(substitutor.sub("&gt;") === ">")

  it should "replace &quot; with \"" in
    assert(substitutor.sub("&quot;") === "\"")

  it should "replace &apos; with '" in
    assert(substitutor.sub("&apos;") === "'")

  it should "replace &amp; with &" in
    assert(substitutor.sub("&amp;") === "&")

  it should "reproduce itself if not given any the above contents" in
    assert(substitutor.sub("&fail;") === "&fail;")

  it should "replace itself from within the middle of plain text" in
    assert(substitutor.sub("one &lt; two") === "one < two")

  it should "not replace itself when provided inside an escape block" in
    assert(substitutor.sub("@<&lt;>") === "&lt;")
}