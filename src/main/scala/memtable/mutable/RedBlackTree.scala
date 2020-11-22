package memtable.mutable

class RedBlackTree[V](val root: Node[V]) {

}

object RedBlackTree {
  def apply[V](root: Node[V]): RedBlackTree[V] = new RedBlackTree[V](root)
}


sealed trait Node[V] {
  val color: Color
}

final case class Root[V](value: V, color: Color)

final case class Branch[V](value: V, parent: Node[V], color: Color) extends Node[V]

final case class Leaf[V](value: Option[V], color: Color) extends Node[V]

