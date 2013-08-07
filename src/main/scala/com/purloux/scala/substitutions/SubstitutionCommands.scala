package com.purloux.scala.substitutions

/** Defines commands used to manipulate text arguments */
object SubstitutionCommands {
  import com.purloux.scala.utils.SafeOperations._

  /** Returns a string representation of a parameterized
   *  command with the given id, parameters and arguments
   *
   *  @param id identifier of the command to show
   *  @param params parameter list
   *  @param contents args list
   */
  val showCommand =
    (id : String) =>
    (params: Seq[String]) =>
    (args: Seq[String]) =>
  {
    val paramList = if (params.isEmpty) "" else "(" + params.mkString(",") + ")"
    val argList = if (args.isEmpty) "" else "[" + args.mkString("|") + "]"
    s"@{$id$paramList$argList}"
  }

  /** Returns a string representation of an error
   *  associated with the execution of a given
   *  parameterized command
   *
   *  @param id identifier of the command that failed
   *  @param params parameter list
   *  @param args argument list
   *  @param error error message to display
   */
  private val reportError =
    (id : String) =>
    (params : Seq[String]) =>
    (args : Seq[String]) =>
    (error : String) =>
  {
    showCommand(id + s":error<${error}>")(params)(args)
  }

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
    val showError = reportError(id)(params)(args)
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
      case _ => showError("invalid function name (" + funcId + ")")
    }
  }

  /** Returns a left (singular) or right (plural) argument
   *  based on the value passed as a parameter
   *
   *  @param params single integer value representing a quantity
   *  @param args two strings representing singular and plural forms
   */
  private val pluralize = (params : Seq[String]) => (args : Seq[String]) => {
    val showError = reportError("pluralize")(params)(args)
    if (params.length != 1)
      showError("too many parameters (1 required)")
    else if (args.length != 2)
      showError("invalid content blocks (2 required)")
    else (params(0) safePerform (_.toInt)) match {
      case Some(value) => value match {
        case 1 => args(0)
        case _ => args(1)
      }
      case None => showError("invalid argument ((int) expected)")
    }
  }

  /** Returns a single argument from a binary if-choice argument
   *
   *  @param params single boolean expression or value
   *  @param args 1-2 possible outputs ([true|false] (optional))
   */
  private val ifChoice = (params : Seq[String]) => (args : Seq[String]) => {
    val showError = reportError("if")(params)(args)
    if (params.length < 1)
      showError("missing parameters (1 or 3 required)")
    else if (args.length < 1)
      showError("missing content blocks (1-2 required)")
    else if (args.length > 2)
      showError("too many content blocks (1-2 required)")
    else {
      if (params.length == 1) (params(0).toString.toLowerCase) match {
        case s if (s == "true") => args(0)
        case s if (s == "false") => if (args.length == 2) args(1) else ""
        case _ => showError("invalid argument ((boolean) expected)")
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
  private val select = (params : Seq[String]) => (args : Seq[String]) => {
    val showError = reportError("select")(params)(args)
    if (params.length < 3)
      showError("missing parameters (3+ required)")
    else if (args.length < params.length - 2)
      showError(s"missing content blocks (${params.length-2}-${params.length-1} required)")
    else if (args.length > params.length - 1)
      showError(s"too many content blocks (${params.length-2}-${params.length-1} required)")
    else
      selectCase("select")(params)(args)
  }

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
    val showError = reportError(id)(params)(args)
    if (params.length != 2)
      showError("invalid parameters (2 required)")
    else if (args.length != 0)
      showError("invalid content blocks (0 allowed)")
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
    val showError = reportError(id)(params)(args)
    createComparison(id)(params)(args){ (l, r) => {
      (l.safePerform(_.toDouble), r.safePerform(_.toDouble)) match {
        case (Some(left), Some(right)) => compare(left, right).toString
        case _ => showError("invalid parameters ((double, double) expected)")
      }
    }}
  }

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
  private val duplicate =
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

  /** Returns "true" or "false" depending on the equality of two
   *  provided values
   *
   *  @param params exactly two values to compare for equality
   *  @param args argument list (unused)
   */
  private val equal = (params : Seq[String]) => (args : Seq[String]) =>
    createComparison("eq")(params)(args){
      (l, r) => { (l.trim.toLowerCase == r.trim.toLowerCase).toString }
    }

  /** Returns "true" or "false" if the first argument has a lesser
   *  numerical value than that of the second argument
   *
   *  @param params exactly two numerical values to compare
   *  @param args argument list (unused)
   */
  private val lessThan = (params : Seq[String]) => (args : Seq[String]) =>
    createNumericComparison("lt")(params)(args){ (l, r) => l < r }

  /** Returns "true" or "false" if the first argument has a greater
   *  numerical value than that of the second argument
   *
   *  @param params exactly two numeric values to compare
   *  @param args argument list (unused)
   */
  private val greaterThan = (params : Seq[String]) => (args : Seq[String]) =>
    createNumericComparison("gt")(params)(args){ (l, r) => l > r }

  /** Returns "true" or "false" if the first argument has a lesser
   *  or equal numerical value than that of the second argument
   *
   *  @param params exactly two numerical values to compare
   *  @param args argument list (unused)
   */
  private val lessOrEqual = (params : Seq[String]) => (args : Seq[String]) =>
    createNumericComparison("lt")(params)(args){ (l, r) => l <= r }

  /** Returns "true" or "false" if the first argument has a greater
   *  or equal numerical value than that of the second argument
   *
   *  @param params exactly two numerical values to compare
   *  @param args argument list (unused)
   */
  private val greaterOrEqual = (params : Seq[String]) => (args : Seq[String]) =>
    createNumericComparison("gt")(params)(args){ (l, r) => l >= r }

  /** Returns the values of a list of arguments updated
   *  with a provided manipulation function
   *
   *  @param fn function to update strings with
   *  @param args argument list to apply manipulations to
   */
  private val updateWith = (fn: (String) => String) => (args: Seq[String]) =>
    args.map(fn).mkString(" ")

  /** Table of unparameterized command-id -> manipulation-functions */
  private val commands = Map[String, Seq[String] => String](
    "caps"    -> updateWith(_.toLowerCase.capitalize),
    "upper"   -> updateWith(_.toUpperCase),
    "lower"   -> updateWith(_.toLowerCase),
    "reverse" -> updateWith(_.reverse)
  )

  /** Table of parameterized command-id -> manipulation-functions */
  private val paramCommands = Map[String, Seq[String] => Seq[String] => String](
    "pluralize" -> (pluralize),
    "select"    -> (select),
    "if"        -> (ifChoice),
    "dup"       -> (duplicate),
    "eq"        -> (equal),
    "lt"        -> (lessThan),
    "lte"       -> (lessOrEqual),
    "gt"        -> (greaterThan),
    "gte"       -> (greaterOrEqual)
  )

  /** Returns an Option for an unparameterized command function
   *  with the given id. None is returned if no command exists
   *  with the provided id
   *
   *  @param id identifier of the command to retrieve
   */
  def getCommand(id : String) : Option[(Seq[String] => String)] =
    commands.get(id)

  /** Returns an Option for a parameterized command function
   *  with the given id. None is returned if no command exists
   *  with the provided id
   *
   *  @param id identifier of the command to retrieve
   */
  def getParamCommand(id : String) : Option[(Seq[String] => Seq[String] => String)] =
    paramCommands.get(id)


}