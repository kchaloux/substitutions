package com.purloux.scala.substitutions

/** Type aliases for command functions */
object CommandTypes {
  type Command = Seq[String] => String
  type ParamCommand = Seq[String] => Seq[String] => String
}

/** Defines commands used to manipulate text arguments */
object SubstitutionCommands {
  import com.purloux.scala.substitutions.commands._

  /** Table of unparameterized command-id -> manipulation-functions */
  private val commands = Map[String, Seq[String] => String](
    "caps"    -> Manipulations.caps,
    "upper"   -> Manipulations.upper,
    "lower"   -> Manipulations.lower,
    "reverse" -> Manipulations.reverse
  )

  /** Table of parameterized command-id -> manipulation-functions */
  private val paramCommands = Map[String, Seq[String] => Seq[String] => String](
    "plural"    -> Branching.plural,
    "select"    -> Branching.select,
    "if"        -> Branching.ifChoice,
    "join"      -> Manipulations.join,
    "dup"       -> Generations.duplicate,
    "eq"        -> Comparisons.equal,
    "lt"        -> Comparisons.lessThan,
    "lte"       -> Comparisons.lessOrEqual,
    "gt"        -> Comparisons.greaterThan,
    "gte"       -> Comparisons.greaterOrEqual,
    "not"       -> Comparisons.notComparison
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