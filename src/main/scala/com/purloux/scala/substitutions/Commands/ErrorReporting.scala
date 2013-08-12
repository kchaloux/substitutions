package com.purloux.scala.substitutions.commands

/** Defines functions for reporting errors in function calls */
object ErrorReporting {

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
  val reportError =
    (id : String) =>
    (params : Seq[String]) =>
    (args : Seq[String]) =>
    (error : String) =>
  {
    showCommand(id + s":error<${error}>")(params)(args)
  }
}