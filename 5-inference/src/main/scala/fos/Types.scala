package fos

// Note that TypeTree is distinct from Type.
// See a comment on TypeTree to learn more.
abstract class Type

case class TypeVar(name: String) extends Type {
  override def toString() = name
}

case class FunType(t1: Type, t2: Type) extends Type {
  override def toString() = "(" + t1 + " -> " + t2 + ")"
}

case object NatType extends Type {
  override def toString() = "Nat"
}

case object BoolType extends Type {
  override def toString() = "Bool"
}

