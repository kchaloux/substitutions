package com.purloux.scala.substitutions

/** Exceptions unique to this Substitutions Library */
object SubstitutionExceptions {

  /** Thrown when parser encounters a NoSuccess Result
   *
   *  @param message information returned by NoSuccess
   *  @param input string that resulted in the parser failure
   */
  case class SubstitutionParserException(message : String, input : String)
    extends Exception(s"Parser Failure: `$message` in `$input`")

  /** Thrown when any command with or without parameters is incapable
   *  of being executed due to invalid inputs or unforseen consequences
   *
   *  @param message information returned by the failed invocation
   */
  abstract class SubstitutionInvocationException(message : String, input : String)
    extends Exception(s"`$message` in `$input`")

  /** Thrown when unparameterized command cannot be executed
   *  due in general to invalid provided input
   *
   *  @param message information returned by failed command invocation
   */
  case class CommandInvocationException(message : String, input : String)
    extends SubstitutionInvocationException(message, input)

  /** Thrown when a parameterized command cannot be executed
   *  due in general to invalid provided arguments
   *
   *  @param message information returned by failed paramCommand invocation
   */
  case class ParamCommandInvocationException(message : String, input : String)
    extends SubstitutionInvocationException(message, input)

  /** Thrown when any command with or without parameters is referenced
   *  that has no existing implementation on the current substitutor
   *
   *  @param commandId identifier of the unknown command
   *  @param message message to display before the identifier
   */
  abstract class UnknownSubstitutionCommandException(commandId: String, message : String)
    extends Exception(s"$message: $commandId")

  /** Thrown when an unparameterized command is referenced
   *  that has no existing implementation
   *
   *  @param commandId identifier of the unknown command */
  case class UnknownCommandException(commandId : String)
    extends UnknownSubstitutionCommandException(commandId, "Unknown Command")

  /** Thrown when a parameterized command is referenced
   *  that has no existing implementation
   *
   *  @param paramCommandId identifier of the unknown command */
  case class UnknownParamCommandException(paramCommandId : String)
    extends UnknownSubstitutionCommandException(paramCommandId, "Unknown Parameterized Command")
}