package fos

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional

case class True() extends Term {
  override def toString() = "true"
}

case class False() extends Term {
  override def toString() = "false"
}
case class Zero() extends Term {
  override def toString() = "0"
}
case class Succ(t: Term) extends Term {
  override def toString() = "succ " + t
}
case class Pred(t: Term) extends Term {
  override def toString() = "pred " + t
}
case class IsZero(t: Term) extends Term {
  override def toString() = "iszero " + t
}
case class If(cond: Term, t1: Term, t2: Term) extends Term {
  override def toString() = "if " + cond + " then " + t1 + " else " + t2
}

case class Var(name: String) extends Term {
  override def toString() = name
}
case class Abs(v: String, tp: Type, t: Term) extends Term {
  override def toString() = "(\\" + v + ":" + tp + "." + t + ")"
}
case class App(t1: Term, t2: Term) extends Term {
  override def toString() = t1.toString + (t2 match {
    case App(_, _) => " (" + t2.toString + ")" // left-associative
    case _         => " " + t2.toString
  })
}
case class TermPair(t1: Term, t2: Term) extends Term {
  override def toString() = "{" + t1 + "," + t2 + "}"
}

case class First(t: Term) extends Term {
  override def toString() = "fst " + t
}

case class Second(t: Term) extends Term {
  override def toString() = "snd " + t
}

/** Abstract Syntax Trees for types. */
abstract class Type extends Positional

case object TypeBool extends Type {
  override def toString() = "Bool"
}
case object TypeNat extends Type {
  override def toString() = "Nat"
}
case class TypeFun(t1: Type, t2: Type) extends Type {
  override def toString() = (t1 match {
    case TypeFun(_, _) => "(" + t1 + ")" // right-associative
    case _             => t1.toString
  }) + "->" + t2
}
case class TypePair(t1: Type, t2: Type) extends Type {
  override def toString() = "(" + t1 + " * " + t2 + ")"
}
