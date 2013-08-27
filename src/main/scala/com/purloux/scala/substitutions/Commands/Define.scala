package com.purloux.scala.substitutions.commands

/** Convenience functions for defining both parameterized
 *  and unparameterized functions for the use of a Substitutor
 */
object Define {

  /** Returns a function that maps a given transformation over a
   *  sequence of strings, then joins them with the given delimiter
   *
   *  @param delim delimiter used to join the results together
   *  @param fn function used to transform each string
   */
  def command(delim : String)(fn : String => String) =
    (contents : Seq[String]) => contents.map(fn).mkString(delim)
  
  /** As command(delim)(fn), using a single space as delimiter */
  def command(fn : String => String): (Seq[String] => String) =
    command(" ")(fn)

  /** Returns a function that maps a given transformation over a 
   *  sequence of strings, providing some list of arguments that
   *  affect how those strings are manipulated, then joins them with
   *  the given delimiter
   *  
   *  @param delim delimiter used to join the results together
   *  @param fn function used to transform each string
   */
  def paramCommand(delim : String)(fn : (Seq[String], String) => String) =
    (args : Seq[String]) => (contents : Seq[String]) =>
      contents.map(str => fn(args, str)).mkString(delim)

  /** As paramCommand(delim)(fn), using a single space as delimiter */
  def paramCommand(fn : (Seq[String], String) => String): (Seq[String] => Seq[String] => String) =
    paramCommand(" ")(fn) 
}