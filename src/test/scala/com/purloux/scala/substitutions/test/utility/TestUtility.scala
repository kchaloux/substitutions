package com.purloux.scala.substitutions.test.utility
import com.purloux.scala.substitutions.SubstitutionParser._
import com.purloux.scala.substitutions.SubstitutionElements._
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.MatchResult

object ParserMatchers {

  def MatchListElements(parserName : String, parser : Parser[ElementsList]) = {
    Matcher { (input : (String, Seq[String])) =>
      val inputText = input._1
      val expectedElements = input._2
      MatchResult (
        parseAll(parser, inputText) match {
          case Success(lst, _) => {
            val actualElements = lst.blocks.map(_.elements.map(_.substituteEscape).mkString)
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

  def MatchParser[A <: SubstitutionElement](
    parserName : String,
    parser : Parser[A],
    validate : (A, String) => Boolean): Matcher[String] =
  {
    Matcher { (input : String) =>
      MatchResult (
        parseAll(parser, input) match {
          case Success(elem, _) => validate(elem, input)
          case _                => false
        },
        s"$parserName did not match `$input`",
        s"$parserName successfully matched `$input`"
      )
    }
  }

  def MatchParser[A <: SubstitutionElement](
    parserName : String,
    parser : Parser[A]): Matcher[String] =
  { 
    MatchParser(parserName, parser, { (elem : A, input : String) => true })
  }
}