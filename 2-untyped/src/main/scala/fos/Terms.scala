package fos

import scala.util.parsing.input.Positional 

/** Abstract Syntax Trees for terms. */
class Term
final case class Var(name: String) extends Term
final case class Abs(v: String, t: Term) extends Term
final case class App(t1: Term, t2: Term) extends Term

