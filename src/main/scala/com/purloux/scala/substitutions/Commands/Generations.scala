package com.purloux.scala.substitutions.commands

/** Defines functions that directly manipulate textual arguments */
object Generations {
  import com.purloux.scala.substitutions.commands.CommandReporting._
  import com.purloux.scala.substitutions.SubstitutionExceptions._
  import com.purloux.scala.substitutions.SubstitutionCommands._
  import com.purloux.scala.utils.SafeOperations._

  /** Returns a string duplicated n times, optionally delimited
   *
   *  Each argument in the given argument list will be duplicated
   *  n times where n is the first argument in the parameters list.
   *  The duplicated elements of a single element will be joined
   *  by the first delimiter provided (defaults to "") and the
   *  resultant strings each each argument will be joined on the
   *  second provided delimiter (defaults to " ")
   *
   *  @param args number of times to repeat the argument,
   *    optionally with inner delimiter (defaults to "")
   *    and optionally with outer delimiter (defaults to " ")
   *  @param contents list of elements to duplicate
   */
  val duplicate =
    (args : Seq[String]) =>
    (contents : Seq[String]) =>
  {
    val input = showCommand("dup")(args)(contents)
    if (args.length < 1) {
      val message = "missing arguments (1-3 required)"
      throw new ParamCommandInvocationException(message, input)
    }
    else if (args.length > 3) {
      val message = "too many arguments (1-3 required)"
      throw new ParamCommandInvocationException(message, input)
    }
    else {
      val delim1 = if (args.length >= 2) args(1) else ""
      val delim2 = if (args.length >= 3) args(2) else ""
      (args(0) safePerform (_.toInt)) match {
        case Some(value) => {
          if (value > 0)
            contents.map(Stream.continually(_).take(value).mkString(delim1)).mkString(delim2)
          else
            ""
        }
        case None => { 
          val message = "invalid arguments ((int, str?, str?) expected)"
          throw new ParamCommandInvocationException(message, input)
        }
      }
    }
  }
}