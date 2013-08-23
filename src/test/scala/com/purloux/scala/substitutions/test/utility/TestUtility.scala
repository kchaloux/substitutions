package com.purloux.scala.substitutions.test.utility
import com.purloux.scala.substitutions.SubstitutionElements._
import scala.util.parsing.combinator.RegexParsers
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.MatchResult

case class ParserMatcher[C <: RegexParsers](val parserCombinator : C) {
  def MatchListElements(parserName : String)
    (getParser : C => parserCombinator.Parser[ElementsList]) =
  {
    val parser = getParser(parserCombinator)
    Matcher { (input : (String, Seq[String])) =>
      val inputText = input._1
      val expectedElements = input._2
      MatchResult (
        parserCombinator.parseAll(parser, inputText) match {
          case parserCombinator.Success(lst, _) => {
            val actualElements = lst.blocks.map(_.elements.map(_.substituteEscaped).mkString)
            (expectedElements, actualElements).zipped.map { case(exp, act) => exp == act }
                                              .foldLeft(true)(_ && _)
          }
          case _ => false
        },
        s"Expected elements of ArgumentsList `[" + 
          expectedElements.mkString(", ") + 
          s" did not match actual elements in `${input._1}`",
        s"Expected elements of ArgumentsList successfully matched actual elements of `${input._1}`"
      )
    }
  }

  def MatchValidatedParser[E <: SubstitutionElement]
    (parserName : String)
    (getParser : C => parserCombinator.Parser[E])
    (validate : (E, String) => Boolean): Matcher[String] =
  {
    val parser = getParser(parserCombinator)
    Matcher { (input : String) =>
      MatchResult (
        parserCombinator.parseAll(parser, input) match {
          case parserCombinator.Success(elem, _) => validate(elem, input)
          case _                                 => false
        },
        s"$parserName did not match `$input`",
        s"$parserName successfully matched `$input`"
      )
    }
  }

  def MatchParser[E <: SubstitutionElement]
    (parserName : String)
    (getParser : C => parserCombinator.Parser[E]): Matcher[String] =
  { 
    MatchValidatedParser(parserName)(getParser) { (a, b) => true }
  }
}