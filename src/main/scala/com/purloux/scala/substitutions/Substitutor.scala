package com.purloux.scala.substitutions
import com.purloux.scala.substitutions.CommandTypes._
import scala.collection.mutable.{ Map => MutableMap }
import com.purloux.scala.utils.SafeOperations._
import scala.util.Random

/** Replaces marked up string contents with new values
 *  given a maintained list of replacement commands and
 *  a map of replacement values
 *
 *  @constructor provide a random number generator for
 *    commands that produce some sort of random output,
 *    a map of unparameterized commands, and
 *    a map of parameterized commands
 */
class Substitutor(rand : Random,
                  cmds : Map[String, Command],
                  paramCmds : Map[String, ParamCommand])
{
  /** @constructor automatic random number generator and default commands */
  def this() = this(new Random(), Map[String, Command](), Map[String, ParamCommand]())

  /** @constructor given random number generator with default commands */
  def this(rand : Random) = this(rand, Map[String, Command](), Map[String, ParamCommand]())
  
  /** @constructor automatic random number generator with specified (un|)parameterized commands */
  def this(cmds: Map[String, Command], paramCmds : Map[String, ParamCommand]) =
    this(new Random(), cmds, paramCmds)

  /** Table of unparameterized command-id -> manipulation-functions */
  private var commands = cmds ++ Map[String, Command](
    "rand" -> (xs => xs(rand.nextInt(xs.length)))
  )

  /** Table of parameterized command-id -> manipulation-functions */
  private var paramCommands = paramCmds

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

  /** Returns a new substitutor with the supplied 
   *  unparameterized command registered to it
   *
   *  @param id identifier of the command to register
   *  @param command function used to manipulation inputs
   */
  def withCommand(id : String, command : Command) =
    new Substitutor(rand, commands ++ Map(id -> command), paramCommands)

  /** Returns a new substitutor with the supplied unparameterized
   *  commands added to its current list of unparameterized commands
   *
   *  @param newCommands map of id -> function commands to add
   */
  def withCommands(newCommands : Map[String, Command]) =
    new Substitutor(rand, commands ++ newCommands, paramCommands)

  /** Returns a new substitutor with the supplied
   *  parameterized commands registered to it
   *
   *  @param id identifier of the command to register
   *  @param command function used to manipulate inputs
   */
  def withParamCommand(id : String, command : ParamCommand) =
    new Substitutor(rand, commands, paramCommands ++ Map(id -> command))

  /** Returns a new substitutor with the supplied parameterized
   *  commands added to its current list of parameterized commands
   *
   *   @param newCommands map of id -> function commands to add
   */
  def withParamCommands(newCommands : Map[String, ParamCommand]) =
      new Substitutor(rand, commands, paramCommands ++ newCommands)

  /** Returns a new substitutor using the given random number generator 
   *  
   *  @param newRandom new RNG to use for random effects generation
   */
  def withRandom(newRandom : Random) = 
    new Substitutor(newRandom, commands, paramCommands)

  /** Returns a new substitutor using a random number generator with
   *  the supplied seed value
   *  
   *  @param seed random number generator seed
   */
  def withRandomSeed(seed : Long) =
    new Substitutor(new Random(seed), commands, paramCommands)

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
  def sub(input : String, args : Map[String, Any]): String = {
    val arguments = args ++ specialCases

    SubstitutionParser.parseAll(SubstitutionParser.wholeText, input) match {
      case SubstitutionParser.Success(output, _) => {
        val result = output.substitute(arguments, this)

        SubstitutionParser.parseAll(SubstitutionParser.wholeText, result) match {
         case SubstitutionParser.Success(escaped, _) => escaped.substituteLast(arguments, this)
         case SubstitutionParser.NoSuccess(_, _) => s"{Parser Failure: ${input}}"
        }
      }
      case SubstitutionParser.NoSuccess(_, _) => s"{Parser Failure: ${input}}"
    }
  }

  /** Returns the substitution result of a string in
   *  multiline mode, using given arguments for replacement values
   *
   *  @param text arbitrary format multiline string to substitute
   *  @param args arguments to use for replacements
   */
  def subMulti(input : String, args : Map[String, Any]): String = {
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

  /** Default implicit substitutor */
  implicit val substitutor = new Substitutor()
}
