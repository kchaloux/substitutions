package com.purloux.scala.substitutions.commands

/** Defines functions that directly manipulate textual arguments */
object Manipulations {
  import com.purloux.scala.substitutions.commands.ErrorReporting._
  import com.purloux.scala.substitutions.SubstitutionCommands._
  import com.purloux.scala.utils.SafeOperations._

  /** Returns a string joining each argument, separated by
   *  the given separator. Ignores empty arguments.
   *  
   *  @param params single string used to separate arguments
   *  @args list of arguments to join together 
   */
  val join = 
    (params : Seq[String]) =>
    (args : Seq[String]) =>
  {
    val showError = reportError("join")(params)(args)
    if (params.length != 1)
      showError("invalid parameters ((str) expected)")
    else
      args.filterNot(_.isEmpty).mkString(params(0))
  }

  /** Returns the values of a list of arguments updated
   *  with a provided manipulation function
   *
   *  @param fn function to update strings with
   *  @param args argument list to apply manipulations to
   */
  private val updateWith = (fn: (String) => String) => (args: Seq[String]) =>
    args.map(fn).mkString(" ")

  /** Returns the provided arguments capitalized */
  val caps = updateWith(_.toLowerCase.capitalize)

  /** Returns the provided arguments in uppercase */
  val upper = updateWith(_.toUpperCase)

  /** Returns the provided arguments in lowercase */
  val lower = updateWith(_.toLowerCase)

  /** Returns the provided arguments individually reversed */
  val reverse = updateWith(_.reverse)
}