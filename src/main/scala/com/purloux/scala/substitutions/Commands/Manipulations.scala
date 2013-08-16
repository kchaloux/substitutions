package com.purloux.scala.substitutions.commands

/** Defines functions that directly manipulate textual arguments */
object Manipulations {
  import com.purloux.scala.substitutions.commands.CommandReporting._
  import com.purloux.scala.substitutions.SubstitutionExceptions._
  import com.purloux.scala.substitutions.SubstitutionCommands._
  import com.purloux.scala.utils.SafeOperations._

  /** Returns a string joining each argument, separated by
   *  the given separator. Ignores empty arguments.
   *  
   *  @param args single string used to separate arguments
   *  @contents list of arguments to join together 
   */
  val join = 
    (args : Seq[String]) =>
    (contents : Seq[String]) =>
  {
    val input = showCommand("join")(args)(contents)
    if (args.length != 1) {
      val message = "invalid parameters ((str) expected)"
      throw new ParamCommandInvocationException(message, input)
    }
    else
      contents.filterNot(_.isEmpty).mkString(args(0))
  }

  /** Returns the values of a list of arguments updated
   *  with a provided manipulation function
   *
   *  @param fn function to update strings with
   *  @param contents argument list to apply manipulations to
   */
  private val updateWith = (fn: (String) => String) => (contents: Seq[String]) =>
    contents.map(fn).mkString(" ")

  /** Returns the provided arguments capitalized */
  val caps = updateWith(_.toLowerCase.capitalize)

  /** Returns the provided arguments in uppercase */
  val upper = updateWith(_.toUpperCase)

  /** Returns the provided arguments in lowercase */
  val lower = updateWith(_.toLowerCase)

  /** Returns the provided arguments individually reversed */
  val reverse = updateWith(_.reverse)
}