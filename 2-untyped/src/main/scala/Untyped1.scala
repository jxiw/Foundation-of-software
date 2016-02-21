package fos

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._

/**
 * This object implements a parser and evaluator for the
 *  untyped lambda calculus found in Chapter 5 of
 *  the TAPL book.
 */
object Untyped1 extends StandardTokenParsers {
  lexical.delimiters ++= List("(", ")", "\\", ".")
  import lexical.Identifier

  /**
   * t ::= x
   * | '\' x '.' t
   * | t t
   * | '(' t ')'
   */
  def term: Parser[Term] = (
    rep1(genericTerm) ^^ { case list => list.reduceLeft { (t1, t2) => App(t1, t2) } }
    | failure("illegal expression")
    )

  def genericTerm = (
    ident ^^ Var
    | "\\" ~> ident ~ "." ~ term ^^ { case x ~ _ ~ t => Abs(x, t) }
    | "(" ~> term <~ ")"
    )


  /**
   * <p>
   *    Alpha conversion: term <code>t</code> should be a lambda abstraction
   *    <code>\x. t</code>.
   *  </p>
   *  <p>
   *    All free occurences of <code>x</code> inside term <code>t/code>
   *    will be renamed to a unique name.
   *  </p>
   *
   *  @param t the given lambda abstraction.
   *  @return  the transformed term with bound variables renamed.
   */
  def alpha(t: Term): Term = {
    def rename(t: Term, z: String): Term = t match {
      case App(t1, t2) => App(rename(t1, z), rename(t2, z))
      case Abs(x, t1) if x != z => Abs(x, rename(t1, z))
      case Var(x) if x == z => Var(x + "1")
      case Var(x) if x != z => Var(x)
      case t => t
    }
    t match {
      case Abs(z, t1) => Abs((z + "1"), rename(t1, z)) 
    }
  }
   

  def FV(t: Term): Set[String] = t match {
    case Var(x)      => Set(x)
    case Abs(x, t)   => FV(t) - x
    case App(t1, t2) => FV(t1) ++ FV(t2)
  }  
  
 
  /**
   * Straight forward substitution method
   *  (see definition 5.3.5 in TAPL book).
   *  [x -> s]t
   *
   *  @param t the term in which we perform substitution
   *  @param x the variable name
   *  @param s the term we replace x with
   *  @return  ...
   */
  def subst(t: Term, x: String, s: Term): Term = t match {
    case Var(y) if y==x => s
    case Var(y) if y!=x => t
    case Abs(y, t1) if y==x => t
    case Abs(y, t1) if (y!=x && !FV(s).contains(y)) => Abs(y, subst(t1, x, s))
    case Abs(y, t1) if (y!=x && FV(s).contains(y)) => subst(alpha(t), x, s)
    case App(t1, t2) => App(subst(t1, x, s), subst(t2, x, s))
  }
  

  /** Term 't' does not match any reduction rule. */
  case class NoReductionPossible(t: Term) extends Exception(t.toString)

  /**
   * Normal order (leftmost, outermost redex first).
   *
   *  @param t the initial term
   *  @return  the reduced term
   */
  
  // the catch used in the following code is to deal with this problem: \x. \y. (x y)((\x. x) y)
  def reduceNormalOrder(t: Term): Term = t match {
    case Abs(y, t1) => Abs(y, reduceNormalOrder(t1))
    case App(Abs(y, t1), t2) => subst(t1, y, t2)
    case App(t1@Var(_), t2) => App(t1, reduceNormalOrder(t2))
    case App(t1@App(_,_), t2) => try {App(reduceNormalOrder(t1), t2)} catch { case NoReductionPossible(_) => App(t1, reduceNormalOrder(t2))}
    case _ => throw NoReductionPossible(t)
  }

  /** Call by value reducer. */
  def reduceCallByValue(t: Term): Term = t match {
    case App(t1@App(_,_), t2) => App(reduceCallByValue(t1), t2)
    case App(t1@Abs(_,_), t2@App(_,_)) => App(t1, reduceCallByValue(t2))
    case App(Abs(y,t1), t2@Abs(_,_)) => subst(t1, y, t2)
    case _ => throw NoReductionPossible(t)
  }

  /**
   * Returns a stream of terms, each being one step of reduction.
   *
   *  @param t      the initial term
   *  @param reduce the method that reduces a term by one step.
   *  @return       the stream of terms representing the big reduction.
   */
  def path(t: Term, reduce: Term => Term): Stream[Term] =
    try {
      var t1 = reduce(t)
      Stream.cons(t, path(t1, reduce))
    } catch {
      case NoReductionPossible(_) =>
        Stream.cons(t, Stream.empty)
    }

  def main(args: Array[String]): Unit = {
    val stdin = new java.io.BufferedReader(new java.io.InputStreamReader(System.in))
    val tokens = new lexical.Scanner(stdin.readLine())
    phrase(term)(tokens) match {
      case Success(trees, _) =>
        println("normal order: ")
        for (t <- path(trees, reduceNormalOrder))
          println(t)
        println("call-by-value: ")
        for (t <- path(trees, reduceCallByValue))
          println(t)

      case e =>
        println(e)
    }
  }
}
