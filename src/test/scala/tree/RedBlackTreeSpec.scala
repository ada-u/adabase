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

        tree.tree.get match {
          case Branch(Branch(ll, lv, lr, lc), v, Branch(rl, rv, rr, rc), c) =>
            assert(v == 2L)
            assert(lv == 1L)
            assert(rv == 3L)
            assert(c == Black)
            assert(lc == Black)
            assert(rc == Black)
          case _ => fail("tree doesn't have 3 branches.")
        }
      }
    }
    // left: red, left->right: red
    "which is inserted 5, 3, 7, 4 in order" - {
      "should be its 4 is red and the others are black." in {
        val tree = RedBlackTree
          .empty[Long]
          .insert(5)
          .insert(3)
          .insert(7)
          .insert(4)

        tree.tree.get match {
          case Branch(Branch(Leaf, lv, Branch(Leaf, lrv, Leaf, lrc), lc), v, Branch(Leaf, rv, Leaf, rc), c) =>
            assert(v == 5)
            assert(lv == 3)
            assert(lrv == 4)
            assert(rv == 7)
            assert(c == Black)
            assert(lc == Black)
            assert(rc == Black)
            assert(lrc == Red)

          case _ => fail("tree doesn't have 4 branches.")
        }
      }
    }
    // right: red, right->left: red
    "which is inserted 5, 3, 7, 6 in order" - {
      "should be its 6 is red and the others are black." in {
        val tree = RedBlackTree
          .empty[Long]
          .insert(5)
          .insert(3)
          .insert(7)
          .insert(6)

        tree.tree.get match {
          case Branch(Branch(Leaf, lv, Leaf, lc), v, Branch(Branch(Leaf, rlv, Leaf, rlc), rv, Leaf, rc), c) =>
            assert(v == 5)
            assert(lv == 3)
            assert(rv == 7)
            assert(rlv == 6)
            assert(c == Black)
            assert(lc == Black)
            assert(rc == Black)
            assert(rlc == Red)

          case _ => fail("tree doesn't have 4 branches.")
        }
      }
    }

    // right: red, right->right: red
    "which is inserted 1, 2, 3, 4 in order" - {
      "should be 4 is red, and the others are black." in {
        val tree = RedBlackTree
          .empty[Long]
          .insert(1)
          .insert(2)
          .insert(3)
          .insert(4)

        tree.tree.get match {
          case Branch(Branch(Leaf, lv, Leaf, lc), v, Branch(Leaf, rv, Branch(rrr, rrv, rrl, rrc), rc), c) =>
            assert(v == 2)
            assert(lv == 1)
            assert(rv == 3)
            assert(rrv == 4)
            assert(c == Black)
            assert(lc == Black)
            assert(rc == Black)
            assert(rrc == Red)

          case _ => fail("tree doesn't have 4 branches.")
        }
      }
    }
  }
}
