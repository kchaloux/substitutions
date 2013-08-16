package com.purloux.scala.substitutions.commands

/** Defines functions that select a branch from a list of arguments */
object Branching {
  import com.purloux.scala.substitutions.commands.CommandReporting._
  import com.purloux.scala.substitutions.SubstitutionExceptions._
  import com.purloux.scala.substitutions.SubstitutionCommands._
  import com.purloux.scala.utils.SafeOperations._

  /** Returns a single argument from a contents list
   *  matching the first True expression in the arguments
   *
   *  @param id identifier of the specific select case command
   *  @param args argument list of boolean expressions
   *  @param contents arguments to select result from
   */
  private val selectCase =
    (id : String) =>
    (args : Seq[String]) =>
    (contents : Seq[String]) =>
  {
    val funcId = args(0)
    val compareWith = args(1)
    getParamCommand(funcId) match {
      case Some(func) => {
        val results = (Stream.continually(compareWith), args.drop(2))
                      .zipped
                      .map{case(x,y) => func(Seq(x, y))(Seq[String]())}
        val choices = results.zip(contents).filter{ case(x,y) => x == "true" }
        if (choices.isEmpty) contents.length match {
          case len if (len > args.length - 2) => contents.last
          case _ => ""
        }
        else choices(0)._2
      }
      case _ => throw new UnknownParamCommandException(funcId)
    }
  }

  /** Returns a left (singular) or right (plural) argument
   *  based on the value passed as a argument
   *
   *  @param args single integer value representing a quantity
   *  @param contents two strings representing singular and plural forms
   */
  val plural = (args : Seq[String]) => (contents : Seq[String]) => {
    val input = showCommand("plural")(args)(contents)
    if (args.length != 1) {
      val message = "too many arguments (1 required)"
      throw new ParamCommandInvocationException(message, input)
    }
    else if (contents.length != 2) {
      val message = "invalid content blocks (2 required)"
      throw new ParamCommandInvocationException(message, input)
    }
    else (args(0) safePerform (_.toInt)) match {
      case Some(value) => value match {
        case 1 => contents(0)
        case _ => contents(1)
      }
      case None => { 
        val message = "invalid argument ((int) expected)"
        val input = showCommand("plural")(args)(contents)
        throw new ParamCommandInvocationException(message, input)
      }
    }
  }

  /** Returns a single argument from a binary if-choice argument
   *
   *  @param args single boolean expression or value
   *  @param contents 1-2 possible outputs ([true|false] (optional))
   */
  val ifChoice = (args : Seq[String]) => (contents : Seq[String]) => {
    val input = showCommand("if")(args)(contents)
    if (args.length < 1) {
      throw new ParamCommandInvocationException("missing arguments (1 or 3 required)", input)
    }
    else if (contents.length < 1) {
      throw new ParamCommandInvocationException("missing content blocks (1-2 required)", input)
    }
    else if (contents.length > 2) {
      throw new ParamCommandInvocationException("too many content blocks (1-2 required)", input)
    }
    else {
      if (args.length == 1) (args(0).toString.toLowerCase) match {
        case s if (s == "true")   => contents(0)
        case s if (s == "false")  => if (contents.length == 2) contents(1) else ""
        case s if (s == s)        => {
          val message = "invalid arugment ((boolean) expected - \"$s\" received)"
          throw new ParamCommandInvocationException(message, input)
        }
      }
      else selectCase("if")(args)(contents)
    }
  }

  /** Returns the first argument matching the index of the first
   *  true boolean expression passed to the arguments
   *
   *  @param args id of a matching function, value to compare against,
   *    followed by a list of values to compare to
   *  @param contents n-n+1 output arguments corresponding to each expression
   *    where n is the number of comparison values provided
   */
  val select = (args : Seq[String]) => (contents : Seq[String]) => {
    val input = showCommand("select")(args)(contents)
    if (args.length < 3) {
      val message = "missing arguments (3+ required)"
      throw new ParamCommandInvocationException(message, input)
    }
    else if (contents.length < args.length - 2) {
      val message = s"missing content blocks (${args.length-2}-${args.length-1} required)"
      throw new ParamCommandInvocationException(message, input)
    }
    else if (contents.length > args.length - 1) {
      val message = s"too many content blocks (${args.length-2}-${args.length-1} required)"
      throw new ParamCommandInvocationException(message, input)
    }
    else
      selectCase("select")(args)(contents)
  }
}