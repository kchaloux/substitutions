package com.purloux.scala.substitutions.commands

/** Defines functions used to compare parameters and return true or false values */
object Comparisons {
  import com.purloux.scala.substitutions.commands.CommandReporting._
  import com.purloux.scala.substitutions.SubstitutionExceptions._
  import com.purloux.scala.substitutions.SubstitutionCommands._
  import com.purloux.scala.substitutions.utils.Extractors._
  import com.purloux.scala.utils.SafeOperations._

  /** Returns a function that compares two values,
   *  or provides an error message for faulty input
   *
   *  @param id identifier of the concrete comparison function
   *  @param args exactly two values to compare
   *  @param contents argument list
   *  @param func actual comparison function
   */
  private val createComparison =
    (id : String) =>
    (args : Seq[Any]) =>
    (contents : Seq[Any]) =>
    (func : (Any, Any) => String) =>
  {
    val input = showCommand(id)(args)(contents)
    if (args.length != 2)
      throw new ParamCommandInvocationException("invalid arguments (2 required)", input)
    else if (contents.length != 0)
      throw new ParamCommandInvocationException("invalid content blocks (0 allowed)", input)
    else
      func(args(0), args(1))
  }

  /** Returns a function that compares two values
   *  that can be treated as comparable numeric values
   *
   *  @param id identifier of the concrete comparison function
   *  @param args exactly two numerically convertible values to compare
   *  @param contents argument list
   *  @param compare comparison function accepting two doubles
   */
  private val createNumericComparison =
    (id : String) =>
    (args : Seq[Any]) =>
    (contents : Seq[Any]) =>
    (compare : (Double, Double) => Boolean) =>
  {
    createComparison(id)(args)(contents){ 
      (l, r) => {
        val input = showCommand(id)(args)(contents)
        val errorMessage = "invalid arguments ((double, double) expected)"
        
        (l, r) match {
          case (FloatingNumber(a), FloatingNumber(b)) => compare(a, b).toString
          case _ => throw ParamCommandInvocationException(errorMessage, input)
        }
      }
    }
  }

  /** Returns "true" or "false" depending on the equality of two
   *  provided values
   *
   *  @param args exactly two values to compare for equality
   *  @param contents argument list (unused)
   */
  val equal = (args : Seq[Any]) => (contents : Seq[Any]) =>
    createComparison("eq")(args)(contents){
      (l, r) => { (l.toString.trim.toLowerCase == r.toString.trim.toLowerCase).toString }
    }

  /** Returns "true" or "false" if the first argument has a lesser
   *  numerical value than that of the second argument
   *
   *  @param args exactly two numerical values to compare
   *  @param contents argument list (unused)
   */
  val lessThan = (args : Seq[Any]) => (contents : Seq[Any]) =>
    createNumericComparison("lt")(args)(contents){ (l, r) => l < r }

  /** Returns "true" or "false" if the first argument has a greater
   *  numerical value than that of the second argument
   *
   *  @param args exactly two numeric values to compare
   *  @param contents argument list (unused)
   */
  val greaterThan = (args : Seq[Any]) => (contents : Seq[Any]) =>
    createNumericComparison("gt")(args)(contents){ (l, r) => l > r }

  /** Returns "true" or "false" if the first argument has a lesser
   *  or equal numerical value than that of the second argument
   *
   *  @param args exactly two numerical values to compare
   *  @param contents argument list (unused)
   */
  val lessOrEqual = (args : Seq[Any]) => (contents : Seq[Any]) =>
    createNumericComparison("lt")(args)(contents){ (l, r) => l <= r }

  /** Returns "true" or "false" if the first argument has a greater
   *  or equal numerical value than that of the second argument
   *
   *  @param args exactly two numerical values to compare
   *  @param contents argument list (unused)
   */
  val greaterOrEqual = (args : Seq[Any]) => (contents : Seq[Any]) =>
    createNumericComparison("gt")(args)(contents){ (l, r) => l >= r }

  /** Returns the inverse of a "true" or "false" argument
   *
   *  @param value "true" or "false" string resulting
   *  @param args list of parameters provided to the parent "notComparison" function
   *  @param contents argument list (unused)
   */
  val notInversion = (value : String) => (args : Seq[Any]) => (contents : Seq[Any]) => {
    if (value == "true")
      "false"
    else if (value == "false")
      "true"
    else {
      val message = "invalid arguments ((boolean) expected - \"$value\" received)"
      val input = showCommand("not")(args)(contents)
      throw new ParamCommandInvocationException(message, input)
    }
  }

  /** Returns the inverse of a given "true" or "false" value, or the
   *  inverse result of a boolean function returning those values
   *
   *  @param args list of parameters including either a boolean string
   *    or the id of a boolean function, followed by two comparison values
   *  @param contents arguments list (unused)
   */
  val notComparison = (args : Seq[Any]) => (contents : Seq[Any]) => {
    val input = showCommand("not")(args)(contents)
    if (contents.length != 0) {
      val message = "invalid content blocks (0 allowed)"
      throw new ParamCommandInvocationException(message, input)
    }
    else {
      if (args.length == 3) {
        val funcId = args(0).toString
        getParamCommand(funcId) match {
          case Some(func) => notInversion(func(args.drop(1))(contents))(args)(contents)
          case _          => throw new UnknownParamCommandException(funcId)
        }
      }
      else if (args.length == 1) {
        notInversion(args(0).toString)(args)(contents)
      }
      else {
        val message = "invalid arguments (1 or 3 required)"
        throw new ParamCommandInvocationException(message, input)
      }
    }
  }
}