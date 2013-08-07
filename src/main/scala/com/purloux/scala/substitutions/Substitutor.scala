package com.purloux.scala.substitutions
import scala.collection.mutable.{ Map => MutableMap }
import com.purloux.scala.utils.SafeOperations._
import scala.util.Random

/** Replaces marked up string contents with new values
 *  given a maintained list of replacement commands and
 *  a map of replacement values
 *
 *  @constructor provide a random number generator for
 *    commands that produce some sort of random output
 */
class Substitutor(rand : Random) {
  type Command = Seq[String] => String
  type ParamCommand = Seq[String] => Seq[String] => String

  /** Construct a substitutor with a new Random providing no special seed */
  def this() = this(new Random)

  /** Updateable table of unparameterized command-id -> manipulation-functions */
  private var commands = MutableMap[String, Command](
    "rand"    -> (xs => xs(rand.nextInt(xs.length)))
  )

  /** Updateable table of parameterized command-id -> manipulation-functions */
  private var paramCommands = MutableMap[String, ParamCommand]()

  /** Returns an Option for an unparameterized command function
   *  with the given id. None is returned if no command exists
   *  with the provided id
   *
   *  @param id identifier of the command to retrieve
   */
  def getCommand(id : String) : Option[Command] =
    SubstitutionCommands.getCommand(id) match {
      case Some(command) => Some(command)
      case None => commands.get(id)
    }

  /** Returns an Option for a parameterized command function
   *  with the given id. None is returned if no command exists
   *  with the provided id
   *
   *  @param id identifier of the command to retrieve
   */
  def getParamCommand(id : String) : Option[ParamCommand] =
    SubstitutionCommands.getParamCommand(id) match {
      case Some(command) => Some(command)
      case None => paramCommands.get(id)
    }

  /** Adds a new command to the unparameterized commands table
   *  with the given id and manipulation function
   *
   *  @param id identifier of the command to register
   *  @param command function used to manipulation inputs
   */
  def registerCommand(id : String, command : Command): Unit =
    commands(id) = command

  /** Adds a new set of commands to the unparameterized commands table
   *
   *  @param newCommands map of id -> function commands to add
   */
  def registerCommands(newCommands : Map[String, Command]): Unit =
    commands ++= newCommands

  /** Adds a new command to the parameterized commands table
   *  with the given id and manipulation function
   *
   *  @param id identifier of the command to register
   *  @param command function used to manipulate inputs
   */
  def registerParamCommand(id : String, command : ParamCommand): Unit =
    paramCommands(id) = command

  /** Adds a new set of commands to the parameterized commands table
   *
   *   @param newCommands map of id -> function commands to add
   */
  def registerParamCommands(newCommands : Map[String, ParamCommand]): Unit =
      paramCommands ++= newCommands


  /** Symbol to prevent automatic whitespace insertion
   *  on newlines for multiline substitution mode
   */
  private val stopSpace = "_"

  /** Special substitution values automatically registered */
  private val specialCases = Map[String, Any](
    stopSpace -> stopSpace,
    "br"      -> "\n"
  )

  /** Returns the substitution result of a string in
   *  standard mode, using given arguments for replacement values
   *
   *  @param text string to substitute
   *  @param args arguments to use for replacements
   */
  def sub(input : String, args : Map[String, Any]) =
    SubstitutionParser.parseAll(SubstitutionParser.wholeText, input) match {
      case SubstitutionParser.Success(output, _) => output.substitute(args ++ specialCases, this)
      case SubstitutionParser.NoSuccess(_, _) => "{Parser Failure: " + input + "}"
    }

  /** Returns the substitution result of a string in
   *  multiline mode, using given arguments for replacement values
   *
   *  @param text arbitrary format multiline string to substitute
   *  @param args arguments to use for replacements
   */
  def subMulti(input : String, args : Map[String, Any]) =
  {
    val withSpace = (str : String) =>
      if (str.endsWith(stopSpace)) str.dropRight(1) else (str + " ")
    val stripped = input.split("(\r)?\n")
                        .map(str => withSpace(str.trim))
                        .mkString
                        .trim

    sub(stripped, args ++ Map("$specialEndTag" -> " "))
  }

  /** Returns the substitution result of a string in
   *  standard mode, providing no replacement arguments
   *
   *  @param text string to substitute
   */
  def sub(text : String): String =
    sub(text, Map[String, Any]())

  /** Returns the substitution result of a string
   *  in multiline mode, providing no replacement arguments
   *
   *  @param text arbitrary format multiline string to substitute
   */
  def subMulti(text : String): String =
    subMulti(text, Map[String, Any]())
}

/** Substitutor Companion Object */
object Substitutor {

  /** Returns a new substitutor with the specified
   *  seed for its internal random number generator
   *
   *  @param seed pseudo-random-number-generator seed
   */
  def initializeWithSeed(seed : Long) = new Substitutor(new Random(seed))
}
