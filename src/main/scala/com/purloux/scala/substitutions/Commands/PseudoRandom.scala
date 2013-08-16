package com.purloux.scala.substitutions.commands

/** Defines functions that directly manipulate textual arguments */
object PseudoRandom {
  import com.purloux.scala.substitutions.commands.CommandReporting._
  import com.purloux.scala.substitutions.SubstitutionExceptions._
  import com.purloux.scala.substitutions.SubstitutionCommands._
  import com.purloux.scala.utils.SafeOperations._
  import scala.util.Random

  /** Returns a random element from a list of elements 
   *
   *  @param rand pseudo random number generator
   *  @param args collection of elements to pull from
   */
  val randElement = (rand : Random) => (args : Seq[String]) => 
    args(rand.nextInt(args.length))

  /** Returns a random BigInt from a range of BigInts 
   *
   *  @param rand pseudo-random number generator
   *  @param params boundaries to pull a value from
   *  @param args content blocks (unused)
   */
  val randNumber =
    (rand : Random) =>
    (params : Seq[String]) =>
    (args : Seq[String]) =>
  {
    val input = showCommand("rand")(params)(args)
    if (params.length != 2) {
      val message = "invalid arguments ((int, int) expected)"
      throw new ParamCommandInvocationException(message, input)
    }
    else if (args.length != 0) {
      val message = "invalid content blocks (0 allowed)"
      throw new ParamCommandInvocationException(message, input)
    }
    else {
      try {
        val boundA = BigInt(params(0).trim)
        val boundB = BigInt(params(1).trim)

        val low = boundA.min(boundB)
        val high = boundA.max(boundB)

        val range = high - low + 1
        val result = (BigInt(255, rand) % range)
        val output = result match {
          case n if n >= range  => high
          case n if n <= 0      => low
          case _                => result + low
        }
        output.toString
      } 
      catch {
        case nf:NumberFormatException => {  
          val message = "invalid parameters ((int, int) expected)"
          throw new ParamCommandInvocationException(message, input)
        }
      }
    }
  }
}
