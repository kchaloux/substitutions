package com.purloux.scala.substitutions.commands

/** Defines functions that directly manipulate textual arguments */
object Generations {
  import com.purloux.scala.substitutions.commands.ErrorReporting._
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
   *  @param params number of times to repeat the argument,
   *    optionally with inner delimiter (defaults to "")
   *    and optionally with outer delimiter (defaults to " ")
   *  @param args list of */
  val duplicate =
    (params : Seq[String]) =>
    (args : Seq[String]) =>
  {
    val showError = reportError("dup")(params)(args)
    if (params.length < 1)
      showError("missing parameters (1-3 required)")
    else if (params.length > 3)
      showError("too many parameters (1-3 required)")
    else {
      val delim1 = if (params.length >= 2) params(1) else ""
      val delim2 = if (params.length >= 3) params(2) else ""
      (params(0) safePerform (_.toInt)) match {
        case Some(value) => {
          if (value > 0)
            args.map(Stream.continually(_).take(value).mkString(delim1)).mkString(delim2)
          else
            ""
        }
        case None => showError("invalid parameters ((int, str?, str?) expected)")
      }
    }
  }
}