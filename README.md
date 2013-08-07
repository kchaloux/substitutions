substitutions
=============

**Substitutions** is a recursive-descent text replacement parser designed to aid in the specification of blocks of text with mutable, conditional, or random segments.

It was created with aiding the development of text-heavy games (such as text adventures), and as an introduction to Scala parser combinators.

## Examples ##

### Simple Replacement ###
*Replace a placeholder element with contents*

```scala
val template = "Welcome, @{name}!"
substitutor.sub(template, Map("name" -> "Jaqen"))

>>> "Welcome, Jaqen!"
```

### Random Selection ###
*Randomly select one of several textual elements*

```scala
val template = "It's such a @{rand[miserable|fine|great]} day."
substitutor.sub(template)

>>> "It's such a miserable day."
  or 
>>> "It's such a fine day."
  or
>>> "It's such a great day."
```

### Basic Transformations ###
*Change the form of a string*

- Uppercase `@{upper[_text_]}`
- Lowercase `@{lower[_text_]}`
- Capitalization `@{caps[_text_]}`
- Reverse `@{reverse[_text_]}`

### Branch Selection ###
*Select sections of text based on "true" or "false" parameters*

```scala
val template = "You're feeling @{if(@{hungry}) [hungry|full]}"
substitutor.subMulti(template, Map("hungry" -> true))
----
>>> "You're feeling hungry"
```

*Match against several possible inputs*

```scala
val template = "That gem is @{select(eq, @{color}, red, green, blue)[a ruby|an emerald|a sapphire]}"
substitutor.sub(template, Map("color" -> red))

>>> "That gem is a ruby" 
```

### Format branch logic without consequence using subMulti ###
*Define complex multiline template strings or text files with ignored newlines and whitespace*

```scala
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
```

*Then apply substitutions with subMulti*

```scala
substitutor.subMulti(template, Map(
	"pro_sub" -> "she",
	"pro_pos" -> "her",
	"weapon" -> "bow",
	"monster_number" -> 5,
	"monster" -> "goblin"))

>>> "She steps into the murky dungeon, drawing her bow at the ready. \nSuddenly, she is ambushed by a gaggle of goblins!"
```

*Provide different inputs to the template to get different outputs*

```scala
substitutor.subMulti(template, Map(
	"pro_sub" -> "he",
	"pro_pos" -> "his",
	"weapon" -> "sword",
	"monster_number" -> 1,
	"monster" -> "dragon"))

>>> "He steps into the gloomy dungeon, drawing his sword at the ready. \nSuddenly, he is ambushed by a lone dragon!"
```

### Extend Substitutors with new commands at Runtime ###
*Provide new text transformation functions to a substitutor*

```scala
// Convert a string to "Title Case"
substitutor.registerCommand("titlecase", {
  _.map { 
    _.split(" ").map {
      _.toLowerCase.capitalize
    }
    .mkString(" ")
  }
  .mkString(" ")
})

val template = "@{titlecase[the tale of twelve tarrasques]}"
substitutor.sub(template)

>>> "The Tale Of Twelve Tarrasques" 
```

To Build
========

The **substitutions** library is built with sbt *(version 0.12.\*)*. Simply clone the latest version of **substitutions** from github, navigate to the project folder, and use the commands:

    > sbt
    > compile

The resulting jar will be generated in the target/scala-2.10/ directory.


Notice
======

This library was built for my own usage and for my own curiosity. No promises are made about the quality or security of the code. 
