package native
package nir

sealed abstract class Op
object Op {
  final case object Undefined                                                extends Op
  final case class Ret    (value: Val)                                       extends Op
  final case class Throw  (value: Val)                                       extends Op
  final case class Jump   (next: Next)                                       extends Op
  final case class If     (value: Val, thenp: Next, elsep: Next)             extends Op
  final case class Switch (value: Val, default: Next, cases: Seq[Case])      extends Op
  final case class Invoke (ptr: Val, args: Seq[Val], succ: Next, fail: Next) extends Op
  final case class Call   (ptr: Val, args: Seq[Val])                         extends Op
  final case class Load   (ty: Type, ptr: Val)                               extends Op
  final case class Store  (ty: Type, ptr: Val, value: Val)                   extends Op
  final case class Elem   (ptr: Val, indexes: Seq[Val])                      extends Op
  final case class Extract(aggr: Val, index: Val)                            extends Op
  final case class Insert (aggr: Val, value: Val, index: Val)                extends Op
  final case class Alloc  (ty: Type)                                         extends Op
  final case class Alloca (ty: Type)                                         extends Op
  final case class Size   (ty: Type)                                         extends Op
  final case class Bin    (bin: nir.Bin, ty: Type, l: Val, r: Val)           extends Op
  final case class Conv   (conv: nir.Conv, ty: Type, value: Val)             extends Op

  //scala
  final case class FieldElem   (name: Name, value: Val) extends Op
  final case class MethodElem  (name: Name, value: Val) extends Op
  final case class AllocClass  (ty: Type)               extends Op
  final case class AllocArray  (ty: Type, length: Val)  extends Op
  final case class Equals      (left: Val, right: Val)  extends Op
  final case class HashCode    (value: Val)             extends Op
  final case class GetClass    (value: Val)             extends Op
  final case class ClassOf     (ty: Type)               extends Op
  final case class AsInstanceOf(value: Val, ty: Type)   extends Op
  final case class IsInstanceOf(value: Val, ty: Type)   extends Op
  final case class ArrayLength (value: Val)             extends Op
  final case class ArrayElem   (value: Val, index: Val) extends Op
  final case class Box         (value: Val, to: Type)   extends Op
  final case class Unbox       (value: Val, to: Type)   extends Op
  final case class MonitorEnter(value: Val)             extends Op
  final case class MonitorExit (value: Val)             extends Op
}
