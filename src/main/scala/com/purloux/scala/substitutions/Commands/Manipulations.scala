package com.purloux.scala.substitutions.commands

/** Defines functions that directly manipulate textual arguments */
object Manipulations {
  import com.purloux.scala.substitutions.commands.CommandReporting._
  import com.purloux.scala.substitutions.SubstitutionExceptions._
  import com.purloux.scala.substitutions.SubstitutionCommands._
  import com.purloux.scala.substitutions.commands.Define._
  import com.purloux.scala.utils.SafeOperations._

  /** Returns a string joining each argument, separated by
   *  the given separator. Ignores empty arguments.
   *  
   *  @param args single string used to separate arguments
   *  @contents list of arguments to join together 
   */
  val join = 
    (args : Seq[Any]) =>
    (contents : Seq[Any]) =>
  {
    val input = showCommand("join")(args)(contents)
    if (args.length != 1) {
      val message = "invalid parameters ((str) expected)"
      throw new ParamCommandInvocationException(message, input)
    }
    else
      contents.map(_.toString).filterNot(_.isEmpty).mkString(args(0).toString)
  }

  /** Returns the provided arguments capitalized */
  val caps = command { _.toString.toLowerCase.capitalize }

  /** Returns the provided arguments in uppercase */
  val upper = command { _.toString.toUpperCase }

  /** Returns the provided arguments in lowercase */
  val lower = command { _.toString.toLowerCase }

  /** Returns the provided arguments individually reversed */
  val reverse = command { _.toString.reverse }
}