

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._
import fos.Abs
import fos.App
import fos.Term
import fos.Var
import scala.Stream

/** This object implements a parser and evaluator for the
 *  untyped lambda calculus found in Chapter 5 of
 *  the TAPL book.
 */
object Untyped2 extends StandardTokenParsers {
  lexical.delimiters ++= List("(", ")", "\\", ".")
  import lexical.Identifier

  /** t ::= x
          | '\' x '.' t
          | t t
          | '(' t ')'
   */
  def term: Parser[Term] = (
        ident ~ term^^{
          case name~term =>App(Var(name),term)}
      | ident^^{case name => Var(name)}
      | "(" ~ term ~ ")" ~ term ^^ { case _~t1~_~t2 => App(t1,t2)}
      
      | "\\" ~ ident ~ "." ~ term^^{
        case _~s~_~ t => Abs(s,t)}
      | "(" ~ term ~ ")" ^^ { case _~t~_ => t }
      
      
      //| term ~ term ^^{ case t1 ~ t2 => App(t1,t2) }
      
  )

  /** <p>
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
  def alpha(t: Term): Term = ???
    
  def isReduce(t: Term):Boolean = t match{
    case Var(x) => false
    case Abs(x,t1) => isReduce(t1)
    case _=> true
  } 

  /** Straight forward substitution method
   *  (see definition 5.3.5 in TAPL book).
   *  [x -> s]t
   *
   *  @param t the term in which we perform substitution
   *  @param x the variable name
   *  @param s the term we replace x with
   *  @return  ...
   */
  def subst(t: Term, x: String, s: Term): Term = t match{
      case Var(y) if y==x => s
      case Var(y) if y!=x => Var(y)
      case Abs(y,t1) if y==x => Abs(y,t1);
      case Abs(y,t1) if y!=x && !(y contains FV(t1)) => Abs(y,subst(t1,x,s))
      case Abs(y,t1) if y!=x && (y contains FV(t1)) => Abs(y,rename(t1,x))
      case App(left,right) => App(subst(left,x,s),subst(right,x,s))
      case _=> throw new NoReductionPossible(t)
  }
  
  def rename(t:Term,x:String):Term = t match{
    case Var(y) => Var("x1")
    case Abs(y,t1) => Abs(y,rename(t1,x))
    case App(t1,t2) => App(rename(t1,x),rename(t2,x))
  }
  
  def FV(t:Term):List[Term] = { 
      val fv=List()
      t match{
        case Var(x) => t +: fv;
        case Abs(x,t) => FV(t) diff List(x) 
        case App(t1,t2) => FV(t1) ++: FV(t2)
    }
  }
  

  /** Term 't' does not match any reduction rule. */
  case class NoReductionPossible(t: Term) extends Exception(t.toString)

  /** Normal order (leftmost, outermost redex first).
   *
   *  @param t the initial term
   *  @return  the reduced term
   */
  def reduceNormalOrder(t: Term): Term = t match {
    case App(Abs(x,t1),t2) => subst(t1,x,t2)
    case App(t1,t2) => App(reduceNormalOrder(t1),reduceNormalOrder(t2))
    case Abs(x,t1) => Abs(x,reduceNormalOrder(t1))
    case Var(x) => Var(x)
    case _=> throw new NoReductionPossible(t)
  }
  /** Call by value reducer. */
  def reduceCallByValue(t: Term): Term = t match {
    case Abs(x,t2) if !t2.isInstanceOf[App]=> Abs(x,reduceCallByValue(t2))
    case App(t1,t2)=> App(reduceCallByValue(t1),t2)
    case Var(x) => Var(x)
    case _=> throw new NoReductionPossible(t)
  }

  /** Returns a stream of terms, each being one step of reduction.
   *
   *  @param t      the initial term
   *  @param reduce the method that reduces a term by one step.
   *  @return       the stream of terms representing the big reduction.
   */
  def path(t: Term, reduce: Term => Term): Stream[Term] =
    try {
      
      if(!isReduce(t))
        throw new NoReductionPossible(t)
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
