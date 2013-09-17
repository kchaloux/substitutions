package com.purloux.scala.substitutions.test.utils
import com.purloux.scala.substitutions.utils.Extractors._
import scala.language.reflectiveCalls
import org.scalatest.FlatSpec
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.ShouldMatchers

class ExtractorsSpec extends FlatSpec with ShouldMatchers {
  lazy val matchWholeNumber = matchExtractor(WholeNumber)
  lazy val matchFloatingNumber = matchExtractor(FloatingNumber)

  "A WholeNumber extractor" should "match int inputs" in {
    1 should matchWholeNumber
  }

  it should "match long inputs" in {
    1L should matchWholeNumber
  }

  it should "match BigInt inputs" in {
    BigInt(1) should matchWholeNumber
  }

  it should "match convertable string inputs" in {
    "123412341234" should matchWholeNumber
  }

  it should "not match non-convertable string inputs" in {
    "abcdefg" should not (matchWholeNumber)
  }

  "A FloatingNumber extractor" should "match int inputs" in {
    1 should matchFloatingNumber
  }

  it should "match long inputs" in {
    1L should matchFloatingNumber
  }

  it should "match BigInt inputs in" in {
    BigInt(1) should matchFloatingNumber
  }

  it should "match convertable string inputs" in {
    "1.512341235" should matchFloatingNumber
  }

  it should "match float inputs" in {
    1F should matchFloatingNumber
  }

  it should "match double inputs" in {
    1D should matchFloatingNumber
  }

  it should "not match non-convertable string inputs" in {
    "abcdefg" should not (matchFloatingNumber)
  }

  def matchExtractor(extractor : {def unapply(a : Any): Option[Any]}) = 
    Matcher { (input : Any) => 
      MatchResult (
        input match {
          case extractor(_) => true
          case _            => false
        },
        s"`$input` did not match the given extractor `${extractor.getClass.getName}`",
        s"`$input` successfully matched extractor `${extractor.getClass.getName}`"
      )
    }
}