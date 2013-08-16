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
   *  @param contents collection of elements to pull from
   */
  val randElement = (rand : Random) => (contents : Seq[String]) => 
    contents(rand.nextInt(contents.length))

  /** Returns a random BigInt from a range of BigInts 
   *
   *  @param rand pseudo-random number generator
   *  @param args boundaries to pull a value from
   *  @param contents content blocks (unused)
   */
  val randNumber =
    (rand : Random) =>
    (args : Seq[String]) =>
    (contents : Seq[String]) =>
  {
    val input = showCommand("rand")(args)(contents)
    if (args.length != 2) {
      val message = "invalid arguments ((int, int) expected)"
      throw new ParamCommandInvocationException(message, input)
    }
    else if (contents.length != 0) {
      val message = "invalid content blocks (0 allowed)"
      throw new ParamCommandInvocationException(message, input)
    }
    else {
      try {
        val boundA = BigInt(args(0).trim)
        val boundB = BigInt(args(1).trim)

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
