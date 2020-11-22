package memtable.mutable.tree

import scala.annotation.tailrec

class RedBlackTree[V: Ordering](private var root: Option[Node[V]]) {

  def size: Long =
    root.map(_.size).getOrElse(0L)

  def insert(value: V)(implicit ord: Ordering[V]): RedBlackTree[V] = {
    @tailrec
    def loop(node: Node[V]): Node[V] = {
      // debug
      node match {
        case Branch(v, _, Leaf, Leaf, c)         => println(s"value: ${v}, left: null, right: null")
        case Branch(v, _, l: Branch[V], Leaf, c) => println(s"value: ${v}, left: ${l.value}, right: null")
        case Branch(v, _, _, r: Branch[V], c)    => println(s"value: ${v}, left: null, right: ${r.value}")
        case Branch(v, _, l, r, c)               => println(s"value: ${v}, left: ${l.value}, right: ${r.value}")
        case Leaf                                => println("leaf")
      }
      node match {
        case Leaf => node
        case _ =>
          if (ord.lt(node.value, /* < */ value)) node match {
            case branch @ Branch(v, _, _, Leaf, Red) =>
              branch.right = Branch(value, Some(branch), Leaf, Leaf, Red)
              rotateLeft(branch)(ord)
              node
            case Branch(v, _, _, right: Branch[V], color) =>
              loop(right)
            case Branch(v, _, left, _, color) =>
              loop(left)
          }
          else if (ord.gt(node.value, value)) node match {
            case branch @ Branch(_, _, _, Leaf, _) =>
              branch.right = Branch(value, Some(branch), Leaf, Leaf, Red)
              //rotateLeft(branch)(ord)
              node
            case Branch(_, _, _, right, _) => loop(right)
          }
          else
            node
      }
    }

    root.fold {
      root = Option(Node.root(value))
    }(loop)

    this
  }

  def rotateLeft(branch: Branch[V])(implicit ord: Ordering[V]): RedBlackTree[V] = {
    println(s"rotateLeft: ${branch.value}")
    branch.left = branch.parent.getOrElse(Leaf)
    branch.parent.foreach {
      case b: Branch[V] =>
        b.parent = Some(branch)
        b.right = Leaf
        //b.color = Red
        root.foreach { r =>
          if (ord.equiv(r.value, b.value)) {
            root = Some(branch)
          }
        }
    }
    //branch.color = Black
    this
  }

  def foreachBreadthFirst[U](f: V => U): Unit = {
    @tailrec
    def loop(acc: Vector[Vector[V]], layer: Vector[Node[V]]): Vector[V] = {
      layer match {
        case Vector() => acc.reverse.flatten
        case _ =>
          loop(
            layer.map(_.value) +: acc,
            layer.flatMap {
              case Leaf                                               => Vector()
              case Branch(_, _, Leaf, Leaf, _)                        => Vector()
              case Branch(_, _, left: Branch[V], Leaf, _)             => Vector(left)
              case Branch(_, _, Leaf, right: Branch[V], _)            => Vector(right)
              case Branch(_, _, left: Branch[V], right: Branch[V], _) => Vector(left, right)
            }
          )
      }
    }
    root.foreach { r =>
      loop(Vector(Vector.empty[V]), Vector(r)).foreach(f)
    }
  }

  private[tree] def rotateLeft(): Unit = {}

  private[tree] def printTree(): Unit = {}

}
object RedBlackTree {
  def empty[V](implicit ord: Ordering[V]): RedBlackTree[V] =
    new RedBlackTree[V](None)
}

sealed trait Node[+V] {
  def value: V
  def color: Color
  def size: Long
}

object Node {
  def root[V](value: V): Branch[V] =
    Branch(value, None, Leaf, Leaf, Black)
}

case class Branch[V](value: V, var parent: Option[Node[V]], var left: Node[V], var right: Node[V], var color: Color) extends Node[V] {
  override def size: Long =
    1 + left.size + right.size
}

case object Leaf extends Node[Nothing] {
  override def value: Nothing = throw new NoSuchElementException("leaf doesn't have a value")
  override def size: Long     = 0L

  override val color: Color = Black
}
