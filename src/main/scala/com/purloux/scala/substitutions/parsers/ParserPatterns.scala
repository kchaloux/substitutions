package com.purloux.scala.substitutions.parsers
import scala.util.parsing.combinator.RegexParsers
import scala.language.postfixOps

/** Defines useful matching functions for RegexParsers */
trait ParserPatterns extends RegexParsers {
  import util.matching.Regex

  // Common regex patterns 
  val start = "^".r
  val end   = "$".r
  val any   = ".*".r
  val space = "\\s*".r

  def quote(elems : String*) : String     = "\\Q" + elems.mkString + "\\E"
  def withAny(elems : String*) : Regex    = ("["  + elems.mkString + "]").r
  def withoutAny(elems : String*) : Regex = ("[^" + elems.mkString + "]").r

   /** Convert a list of strings or characters into a single string */
  def listToString(result: List[java.io.Serializable]): String = result match {
    case (elems) => elems.map {
      _ match {
        case (l~r) => Seq(l, r).mkString("")
        case (any) => any
      }
    }.mkString("")
  }
}