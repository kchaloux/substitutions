package com.purloux.scala.substitutions

/** Defines methods and instances necessary for the implicit 
 *  substitutions of strings using shorthand syntax 
 */
object ImplicitSubstitutions {
  import scala.language.postfixOps

  /** Extend string objects with substitution methods 
   *
   *  @param template implicit string to be substituted
   */
  implicit class StringWithSubstitutions(val template : String) extends AnyVal {
    
    /** Returns the argumentless substitution of a string
     *  using an implicit substitutor
     *
     *  @param substitutor implicit text Substitutor from com.purloux.scala.substitutions
     */
    def sub()(implicit substitutor : Substitutor) =
      substitutor.sub(template)
    
    /** Returns the substitution of a string using the given
     *  arguments and an implicit substitutor
     *
     *  @param args arguments to use for replacements
     * */
    def sub(args : Map[String, Any])(implicit substitutor : Substitutor) =
      substitutor.sub(template, args)

     /** Returns the argumentless substitution of a string
      *  in multiline mode, using an implicit substitutor
      *
      *  @param substitutor implicit text Substitutor from com.purloux.scala.substitutions
      */
    def subMulti()(implicit substitutor : Substitutor) =
      substitutor.subMulti(template)
    
    /** Returns the substitution of a string in multiline mode, 
     *  using the given arguments and an implicit substitutor
     *
     *  @param args arguments to use for replacements
     * */
    def subMulti(args : Map[String, Any])(implicit substitutor : Substitutor) =
      substitutor.subMulti(template, args)
  }
}

/** Defines operator syntax for implicit substitutions */
object ImplicitOperators {
  import scala.language.postfixOps
  
  /** Extend string objects with substitution operators 
   * 
   *  @param template implicit string to be substituted
   */
  implicit class StringWithSubstitutionOps(val template : String) extends AnyVal {
    import ImplicitSubstitutions._

    /** Shorthand for sub(args) */
    def <+ (args : Map[String, Any])(implicit substitutor : Substitutor) = template sub args
    
    /** Shorthand for sub() */
    def <+! (implicit substitutor : Substitutor) = template sub ()
    
    /** Shorthand for subMulti(args) */
    def <++ (args : Map[String, Any])(implicit substitutor : Substitutor) = template subMulti args
    
    /** Shorthand for subMulti() */
    def <++! (implicit substitutor : Substitutor) = template subMulti ()
  }
}
