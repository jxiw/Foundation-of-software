package fos

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
sealed abstract class Term extends Positional
final case object True extends Term
final case object False extends Term
final case class If(cond: Term, t1: Term, t2: Term) extends Term
final case object Zero extends Term
final case class Succ(t: Term) extends Term
final case class Pred(t: Term) extends Term
final case class IsZero(t: Term) extends Term
