package com.purloux.scala.substitutions.commands

/** Defines functions used to compare parameters and return true or false values */
object Comparisons {
  import com.purloux.scala.substitutions.commands.CommandReporting._
  import com.purloux.scala.substitutions.SubstitutionExceptions._
  import com.purloux.scala.substitutions.SubstitutionCommands._
  import com.purloux.scala.utils.SafeOperations._

  /** Returns a function that compares two values,
   *  or provides an error message for faulty input
   *
   *  @param id identifier of the concrete comparison function
   *  @param params exactly two values to compare
   *  @param args argument list
   *  @param func actual comparison function
   */
  private val createComparison =
    (id : String) =>
    (params : Seq[String]) =>
    (args : Seq[String]) =>
    (func : (String, String) => String) =>
  {
    val input = showCommand(id)(params)(args)
    if (params.length != 2)
      throw new ParamCommandInvocationException("invalid arguments (2 required)", input)
    else if (args.length != 0)
      throw new ParamCommandInvocationException("invalid content blocks (0 allowed)", input)
    else
      func(params(0), params(1))
  }

  /** Returns a function that compares two values
   *  that can be treated as comparable numeric values
   *
   *  @param id identifier of the concrete comparison function
   *  @param params exactly two numerically convertible values to compare
   *  @param args argument list
   *  @param compare comparison function accepting two doubles
   */
  private val createNumericComparison =
    (id : String) =>
    (params : Seq[String]) =>
    (args : Seq[String]) =>
    (compare : (Double, Double) => Boolean) =>
  {
    createComparison(id)(params)(args){ (l, r) => {
      (l.safePerform(_.toDouble), r.safePerform(_.toDouble)) match {
        case (Some(left), Some(right)) => compare(left, right).toString
        case _ => {
          val message = "invalid arguments ((double, double) expected)"
          val input = showCommand(id)(params)(args)
          throw new ParamCommandInvocationException(message, input)
        }
      }
    }}
  }

  /** Returns "true" or "false" depending on the equality of two
   *  provided values
   *
   *  @param params exactly two values to compare for equality
   *  @param args argument list (unused)
   */
  val equal = (params : Seq[String]) => (args : Seq[String]) =>
    createComparison("eq")(params)(args){
      (l, r) => { (l.trim.toLowerCase == r.trim.toLowerCase).toString }
    }

  /** Returns "true" or "false" if the first argument has a lesser
   *  numerical value than that of the second argument
   *
   *  @param params exactly two numerical values to compare
   *  @param args argument list (unused)
   */
  val lessThan = (params : Seq[String]) => (args : Seq[String]) =>
    createNumericComparison("lt")(params)(args){ (l, r) => l < r }

  /** Returns "true" or "false" if the first argument has a greater
   *  numerical value than that of the second argument
   *
   *  @param params exactly two numeric values to compare
   *  @param args argument list (unused)
   */
  val greaterThan = (params : Seq[String]) => (args : Seq[String]) =>
    createNumericComparison("gt")(params)(args){ (l, r) => l > r }

  /** Returns "true" or "false" if the first argument has a lesser
   *  or equal numerical value than that of the second argument
   *
   *  @param params exactly two numerical values to compare
   *  @param args argument list (unused)
   */
  val lessOrEqual = (params : Seq[String]) => (args : Seq[String]) =>
    createNumericComparison("lt")(params)(args){ (l, r) => l <= r }

  /** Returns "true" or "false" if the first argument has a greater
   *  or equal numerical value than that of the second argument
   *
   *  @param params exactly two numerical values to compare
   *  @param args argument list (unused)
   */
  val greaterOrEqual = (params : Seq[String]) => (args : Seq[String]) =>
    createNumericComparison("gt")(params)(args){ (l, r) => l >= r }

  /** Returns the inverse of a "true" or "false" argument
   *
   *  @param value "true" or "false" string resulting
   *  @param params list of parameters provided to the parent "notComparison" function
   *  @param args argument list (unused)
   */
  val notInversion = (value : String) => (params : Seq[String]) => (args : Seq[String]) => {
    if (value == "true")
      "false"
    else if (value == "false")
      "true"
    else {
      val message = "invalid arguments ((boolean) expected - \"$value\" received)"
      val input = showCommand("not")(params)(args)
      throw new ParamCommandInvocationException(message, input)
    }
  }

  /** Returns the inverse of a given "true" or "false" value, or the
   *  inverse result of a boolean function returning those values
   *
   *  @param params list of parameters including either a boolean string
   *    or the id of a boolean function, followed by two comparison values
   *  @param args arguments list (unused)
   */
  val notComparison = (params : Seq[String]) => (args : Seq[String]) => {
    val input = showCommand("not")(params)(args)
    if (args.length != 0) {
      val message = "invalid content blocks (0 allowed)"
      throw new ParamCommandInvocationException(message, input)
    }
    else {
      if (params.length == 3) {
        val funcId = params(0)
        getParamCommand(funcId) match {
          case Some(func) => notInversion(func(params.drop(1))(args))(params)(args)
          case _          => throw new UnknownParamCommandException(funcId)
        }
      }
      else if (params.length == 1) {
        notInversion(params(0))(params)(args)
      }
      else {
        val message = "invalid arguments (1 or 3 required)"
        throw new ParamCommandInvocationException(message, input)
      }
    }
  }
}