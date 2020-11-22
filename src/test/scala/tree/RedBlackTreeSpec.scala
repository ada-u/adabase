package tree

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class RedBlackTreeSpec extends AnyFreeSpec with Matchers {

  "A RedBlackTree" - {
    "which is inserted 2, 1 in order" - {
      "should be its root(2) is black and left(1) is red." in {
        val tree = RedBlackTree
          .empty[Long]
          .insert(2L)
          .insert(1L)

        tree.printTree()

        tree.tree.get match {
          case Branch(Branch(Leaf, lv, Leaf, lc), v, Leaf, c)=>
            assert(v == 2L)
            assert(lv == 1L)
            assert(c == Black)
            assert(lc == Red)
          case _ => fail("tree doesn't have 2 branches.")
        }
      }
    }
    "which is inserted 2, 3 in order" - {
      "should be its root(2) is black and right(3) is red." in {
        val tree = RedBlackTree
          .empty[Long]
          .insert(2L)
          .insert(3L)

        tree.printTree()

        tree.tree.get match {
          case Branch(Leaf, v, Branch(Leaf, rv, Leaf, rc), c)=>
            assert(v == 2L)
            assert(rv == 3L)
            assert(c == Black)
            assert(rc == Red)
          case _ => fail("tree doesn't have 2 branches.")
        }
      }
    }
    // left: red, left->left: red
    "which is inserted 3, 2, 1 in order" - {
      "should be its root(2) is black, left(1) is red, and right(3) is also red." in {
        val tree = RedBlackTree
          .empty[Long]
          .insert(3L)
          .insert(2L)
          .insert(1L)

        tree.printTree()

        tree.tree.get match {
          case Branch(Branch(ll, lv, lr, lc), v, Branch(rl, rv, rr, rc), c) =>
            assert(v == 2L)
            assert(lv == 1L)
            assert(rv == 3L)
            assert(c == Black)
            assert(lc == Red)
            assert(rc == Red)
          case _ => fail("tree doesn't have 3 branches.")
        }
      }
    }
    // left: red, left->right: red
    "which is inserted 5, 3, 8, 4 in order" - {
      "should be its root(5) is black, left(1) is red, and right(3) is also red." in {
        val tree = RedBlackTree
          .empty[Long]
          .insert(5)
          .insert(3)
          .insert(8)
          .insert(4)

        tree.printTree()
      }
    }
  }
}
