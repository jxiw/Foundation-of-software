package fos

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._

/** This object implements a parser and evaluator for the NB
 *  language of booleans and numbers found in Chapter 3 of
 *  the TAPL book.
 */
object Arithmetic extends StandardTokenParsers {
  lexical.reserved ++= List("true", "false", "0", "if", "then", "else", "succ", "pred", "iszero")

  import lexical.NumericLit

  /** term ::= 'true'
             | 'false'
             | 'if' term 'then' term 'else' term
             | '0'
             | 'succ' term
             | 'pred' term
             | 'iszero' term
   */
  def parseNum(num:Int):Term={
    if(num==0)
      return Zero
    else
      return Succ(parseNum(num-1))
  }
  
  def term: Parser[Term] = (
        "true" ^^^ True
      | "false" ^^^ False
      | "if" ~ term ~ "then" ~ term ~ "else" ~ term ^^ {case _ ~ cond ~ _ ~ s1 ~ _ ~ s2 => If(cond,s1,s2)}
      | numericLit ^^ {case num => parseNum(num.toInt)}
      | "succ" ~ term ^^ {case _ ~ s => Succ(s)}
      | "pred" ~ term ^^ {case _ ~ s => Pred(s)}
      | "iszero" ~ term ^^ {case _ ~ s => IsZero(s)} 
      )
  
  case class NoReductionPossible(t: Term) extends Exception(t.toString)

  /** Return a list of at most n terms, each being one step of reduction. */
  def path(t: Term, n: Int = 64): List[Term] =
    if (n <= 0) Nil
    else
      t :: {
        try {
          path(reduce(t), n - 1)
        } catch {
          case NoReductionPossible(t1) =>
            Nil
        }
      }

  /** Perform one step of reduction, when possible.
   *  If reduction is not possible NoReductionPossible exception
   *  with corresponding irreducible term should be thrown.
   */
  def reduce(t: Term): Term = t match{
    case If(True,t1,t2) => t1
    case If(False,t1,t2) => t2
    case If(cond,t1,t2) => If(reduce(cond),t1,t2)
    case IsZero(Zero) => True
    case IsZero(Succ(t)) => False
    case IsZero(t) => IsZero(reduce(t))
    case Pred(Zero) => Zero
    case Pred(Succ(t)) => t
    case Pred(t) => Pred(reduce(t))
    case Succ(t) => Succ(reduce(t))
    case _ => throw new NoReductionPossible(t)
  }

  case class TermIsStuck(t: Term) extends Exception(t.toString)

  def isNumberic(t:Term):Boolean=t match{
    case Zero => true
    case Succ(t) => isNumberic(t)
    case Pred(t) => isNumberic(t)
    case _ => false
  }
  
  def isValue(t:Term):Boolean=t match{
    case True => true
    case False => true
    case Zero => true
    case _ => false
  }
  
  /** Perform big step evaluation (result is always a value.)
   *  If evaluation is not possible TermIsStuck exception with
   *  corresponding inner irreducible term should be thrown.
   */
  def eval(t: Term): Term = t match {
    case v if isValue(v) => v
    case If(cond,t1,t2) => eval(cond) match {
      case True => eval(t1)
      case False => eval(t2)
      case _ => throw new TermIsStuck(t)
    }
    case Succ(t) => eval(t) match {
      case num if isNumberic(t) => Succ(num)
      case _ => throw new TermIsStuck(t)
    }
    case Pred(t) => eval(t) match {
      case Zero => Zero
      case Succ(num) if isNumberic(num) => num
      case _ => throw new TermIsStuck(t)
    }
    case IsZero(t) => eval(t) match {
      case Zero => True
      case Succ(num) if isNumberic(num) => False
      case _ => throw new TermIsStuck(t)
    } 
    case _ => throw new TermIsStuck(t)
  }
  

  def main(args: Array[String]): Unit = {
    val stdin = new java.io.BufferedReader(new java.io.InputStreamReader(System.in))
    val tokens = new lexical.Scanner(stdin.readLine())
    phrase(term)(tokens) match {
      case Success(trees, _) =>
        for (t <- path(trees))
          println(t)
        try {
          print("Big step: ")
          println(eval(trees))
        } catch {
          case TermIsStuck(t) => println("Stuck term: " + t)
        }
      case e =>
        println(e)
    }
  }
}
