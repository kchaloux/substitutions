package com.purloux.scala.substitutions.commands

/** Defines functions that select a branch from a list of arguments */
object Branching {
  import com.purloux.scala.substitutions.commands.CommandReporting._
  import com.purloux.scala.substitutions.SubstitutionExceptions._
  import com.purloux.scala.substitutions.SubstitutionCommands._
  import com.purloux.scala.utils.SafeOperations._

  /** Returns a single argument from a contents list
   *  matching the first True expression in the parameters
   *
   *  @param id identifier of the specific select case command
   *  @param params parameter list of boolean expressions
   *  @param args arguments to select result from
   */
  private val selectCase =
    (id : String) =>
    (params : Seq[String]) =>
    (args : Seq[String]) =>
  {
    val funcId = params(0)
    val compareWith = params(1)
    getParamCommand(funcId) match {
      case Some(func) => {
        val results = (Stream.continually(compareWith), params.drop(2))
                      .zipped
                      .map{case(x,y) => func(Seq(x, y))(Seq[String]())}
        val choices = results.zip(args).filter{ case(x,y) => x == "true" }
        if (choices.isEmpty) args.length match {
          case len if (len > params.length - 2) => args.last
          case _ => ""
        }
        else choices(0)._2
      }
      case _ => throw new UnknownParamCommandException(funcId)
    }
  }

  /** Returns a left (singular) or right (plural) argument
   *  based on the value passed as a parameter
   *
   *  @param params single integer value representing a quantity
   *  @param args two strings representing singular and plural forms
   */
  val plural = (params : Seq[String]) => (args : Seq[String]) => {
    val input = showCommand("plural")(params)(args)
    if (params.length != 1) {
      val message = "too many arguments (1 required)"
      throw new ParamCommandInvocationException(message, input)
    }
    else if (args.length != 2) {
      val message = "invalid content blocks (2 required)"
      throw new ParamCommandInvocationException(message, input)
    }
    else (params(0) safePerform (_.toInt)) match {
      case Some(value) => value match {
        case 1 => args(0)
        case _ => args(1)
      }
      case None => { 
        val message = "invalid argument ((int) expected)"
        val input = showCommand("plural")(params)(args)
        throw new ParamCommandInvocationException(message, input)
      }
    }
  }

  /** Returns a single argument from a binary if-choice argument
   *
   *  @param params single boolean expression or value
   *  @param args 1-2 possible outputs ([true|false] (optional))
   */
  val ifChoice = (params : Seq[String]) => (args : Seq[String]) => {
    val input = showCommand("if")(params)(args)
    if (params.length < 1) {
      throw new ParamCommandInvocationException("missing arguments (1 or 3 required)", input)
    }
    else if (args.length < 1) {
      throw new ParamCommandInvocationException("missing content blocks (1-2 required)", input)
    }
    else if (args.length > 2) {
      throw new ParamCommandInvocationException("too many content blocks (1-2 required)", input)
    }
    else {
      if (params.length == 1) (params(0).toString.toLowerCase) match {
        case s if (s == "true")   => args(0)
        case s if (s == "false")  => if (args.length == 2) args(1) else ""
        case s if (s == s)        => {
          val message = "invalid arugment ((boolean) expected - \"$s\" received)"
          throw new ParamCommandInvocationException(message, input)
        }
      }
      else selectCase("if")(params)(args)
    }
  }

  /** Returns the first argument matching the index of the first
   *  true boolean expression passed to the parameters
   *
   *  @param params id of a matching function, value to compare against,
   *    followed by a list of values to compare to
   *  @param args n-n+1 output arguments corresponding to each expression
   *    where n is the number of comparison values provided
   */
  val select = (params : Seq[String]) => (args : Seq[String]) => {
    val input = showCommand("select")(params)(args)
    if (params.length < 3) {
      val message = "missing arguments (3+ required)"
      throw new ParamCommandInvocationException(message, input)
    }
    else if (args.length < params.length - 2) {
      val message = s"missing content blocks (${params.length-2}-${params.length-1} required)"
      throw new ParamCommandInvocationException(message, input)
    }
    else if (args.length > params.length - 1) {
      val message = s"too many content blocks (${params.length-2}-${params.length-1} required)"
      throw new ParamCommandInvocationException(message, input)
    }
    else
      selectCase("select")(params)(args)
  }
}