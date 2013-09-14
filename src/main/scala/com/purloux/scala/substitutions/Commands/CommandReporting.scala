package com.purloux.scala.substitutions.commands

/** Defines functions for displaying commands in string output */
object CommandReporting {

  /** Returns a string representation of a parameterized
   *  command with the given id, parameters and arguments
   *
   *  @param id identifier of the command to show
   *  @param args parameter list
   *  @param contents contents list
   */
  val showCommand =
    (id : String) =>
    (args: Seq[Any]) =>
    (contents: Seq[Any]) =>
  {
    val paramList = if (args.isEmpty) "" else "(" + args.mkString(",") + ")"
    val argList = if (contents.isEmpty) "" else "[" + contents.mkString("|") + "]"
    s"@{$id$paramList$argList}"
  }
}