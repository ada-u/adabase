package tree

// Invariant
// 1. root is always black
// 2. no 2 reds in a row
//   - 1 red node => only black children
// 3.every root-leaf path has same number of black nodes
class RedBlackTree[V: Ordering](val tree: Option[Node[V]]) {

  def insert(newValue: V)(implicit ord: Ordering[V]): RedBlackTree[V] = {

    def build(left: Node[V], value: V, right: Node[V], color: Color): Node[V] = {
      // debug: println(s"left: $left, value: $value, right: $right, color: $color")
      (left, value, right, color) match {
        // right: red, left: red
        case (Branch(ll, lv, lr, Red), v, Branch(rl, rv, rr, Red), _) =>
          Branch(Branch(ll, lv, lr, Black), v, Branch(rl, rv, rr, Black), Red)
        // left: red, left->left: red
        case (Branch(Branch(lll, llv, llr, Red), lv, lr, Red), v, r, _) =>
          Branch(Branch(lll, llv, llr, Black), lv, Branch(lr, v, r, Black), Red)
        // left: red, left->right: red
        case (Branch(ll, lv, Branch(lrl, lrv, lll, Red), Red), v, r, _) =>
          Branch(Branch(ll, lv, lrl, Black), lrv, Branch(lll, v, r, Black), Red)
        // right: red, right->right: red
        case (l, v, Branch(rlr, rlv, Branch(rrr, rrv, rrl, Red), Red), _) =>
          Branch(Branch(l, v, rlr, Black), rlv, Branch(rrr, rrv, rrl, Black), Red)
        // right: red, right->left: red
        case (l, v, Branch(Branch(rll, rlv, rlr, Red), rv, rr, Red), _) =>
          Branch(Branch(l, v, rll, Black), rlv, Branch(rlr, rv, rr, Black), Red)
        case _ =>
          Branch(left, value, right, color)
      }
    }

    def loop(node: Node[V]): Node[V] = {
      node match {
        case Branch(l, v, r, c) =>
          if (ord.lt(newValue, v))
            build(loop(l), v, r, c)
          else if (ord.gt(newValue, v))
            build(l, v, loop(r), c)
          else
            node
        case Leaf =>
          Branch(Leaf, newValue, Leaf, Red)
      }
    }

    val newTree = tree.fold[Node[V]](Branch(Leaf, newValue, Leaf, Black)) { root =>
      loop(root) match {
        case Branch(l, v, r, _) => Branch(l, v, r, Black)
        case leaf @ Leaf        => leaf
      }
    }

    new RedBlackTree[V](Option(newTree))(ord)
  }

  def printTree(): Unit = {
    def valueWithColor(branch: Branch[V]): String =
      s"${branch.value}(${branch.color})"

    // bfs
    def loop(node: Node[V]): Unit =
      node match {
        case Leaf =>
          Leaf
        case Branch(Leaf, value, Leaf, color) =>
          println(s"$value($color)")
        case Branch(left: Branch[V], value, Leaf, color) =>
          println(s"$value($color)")
          println(valueWithColor(left))
          loop(left.left)
        case Branch(Leaf, value, right: Branch[V], color) =>
          println(s"$value($color)")
          println(valueWithColor(right))
          loop(right.right)
        case Branch(left: Branch[V], value, right: Branch[V], color) =>
          println(s"$value($color)")
          print(valueWithColor(left))
          print(" ")
          println(valueWithColor(right))
          loop(left.left)
          loop(left.right)
          loop(right.left)
          loop(right.right)
      }

    if (tree.nonEmpty)
      loop(tree.get)
  }

}

object RedBlackTree {
  def empty[V](implicit ord: Ordering[V]): RedBlackTree[V] =
    new RedBlackTree(None)
}

sealed trait Node[+V]

case class Branch[V](left: Node[V], value: V, right: Node[V], color: Color) extends Node[V] {
  override def toString: String = s"Branch(value: $value, color: $color)"
}

case object Leaf extends Node[Nothing] {
  val color: Color = Black

  override def toString: String = "Leaf(color: Black)"
}

sealed trait Color

case object Red extends Color {
  override def toString: String = "red"
}
case object Black extends Color {
  override def toString: String = "black"
}
