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
  def command(delim : String)(fn : Any => String) =
    (contents : Seq[Any]) => contents.map(fn).mkString(delim)
  
  /** As command(delim)(fn), using a single space as delimiter */
  def command(fn : Any => String): (Seq[Any] => String) =
    command(" ")(fn)

  /** Returns a function that maps a given transformation over a 
   *  sequence of strings, providing some list of arguments that
   *  affect how those strings are manipulated, then joins them with
   *  the given delimiter
   *  
   *  @param delim delimiter used to join the results together
   *  @param fn function used to transform each string
   */
  def paramCommand(delim : String)(fn : (Seq[Any], Any) => String) =
    (args : Seq[Any]) => (contents : Seq[Any]) =>
      contents.map(str => fn(args, str)).mkString(delim)

  /** As paramCommand(delim)(fn), using a single space as delimiter */
  def paramCommand(fn : (Seq[Any], Any) => String): (Seq[Any] => Seq[Any] => String) =
    paramCommand(" ")(fn) 
}