package com.purloux.scala.substitutions

/** Defines implicit substitution inputs that are valid special
 *  cases for various modes of substitution
 */
object SpecialCases {
  /** Symbol to prevent automatic whitespace insertion
   *  on newlines for multiline substitution mode
   */
  val stopSpace = "_"

  /** Special substitution values automatically registered */
  val specialCases = Map[String, Any](
    stopSpace -> stopSpace,
    "br"      -> "\n"
  )
}