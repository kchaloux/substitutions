package com.purloux.scala.substitutions.commands

/** Defines functions for displaying commands in string output */
object CommandReporting {

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
}