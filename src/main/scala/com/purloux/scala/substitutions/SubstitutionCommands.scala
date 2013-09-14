package com.purloux.scala.substitutions

/** Type aliases for command functions */
object CommandTypes {
  type Command = Seq[Any] => String
  type ParamCommand = Seq[Any] => Seq[Any] => String
}

/** Defines commands used to manipulate text arguments */
object SubstitutionCommands {
  import com.purloux.scala.substitutions.commands._
  import com.purloux.scala.substitutions.CommandTypes._

  /** Table of unparameterized command-id -> manipulation-functions */
  private val commands = Map[String, Command](
    "caps"    -> Manipulations.caps,
    "upper"   -> Manipulations.upper,
    "lower"   -> Manipulations.lower,
    "reverse" -> Manipulations.reverse
  )

  /** Table of parameterized command-id -> manipulation-functions */
  private val paramCommands = Map[String, ParamCommand](
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
  def getCommand(id : String) : Option[Command] =
    commands.get(id)

  /** Returns an Option for a parameterized command function
   *  with the given id. None is returned if no command exists
   *  with the provided id
   *
   *  @param id identifier of the command to retrieve
   */
  def getParamCommand(id : String) : Option[ParamCommand] =
    paramCommands.get(id)
}