package com.purloux.scala.substitutions.utils
import com.purloux.scala.substitutions.SubstitutionExceptions._
import com.purloux.scala.utils.SafeOperations._

/** Custom extractors to aid in the creation of substitution functions */
object Extractors {
  /** Extract a whole number as BigInt from any applicable input,
    * including Int, Long, and String 
    */
  object WholeNumber {
    def unapply(a : Any) = a match {
      case i:Int    => Some(BigInt(i))
      case l:Long   => Some(BigInt(l))
      case b:BigInt => Some(b)
      case s:String => s safePerform (value => BigInt(value.trim))
      case _        => None
    }
  }

  /** Extract a floating point number as Double from any applicable input,
    * including Int, Long, String, BigInt, Float and Double
    */
  object FloatingNumber {
    def unapply(a : Any) = a match {
      case WholeNumber(w) => Some(w.toDouble)
      case f:Float        => Some(f.toDouble)
      case d:Double       => Some(d)
      case s:String       => s safePerform (_.toDouble)
      case _              => None
    }
  }
}