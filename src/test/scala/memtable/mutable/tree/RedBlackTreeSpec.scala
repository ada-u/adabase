package memtable.mutable.tree

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper


class RedBlackTreeSpec extends AnyFreeSpec with Matchers {

  "A RedBlackTree" - {
    "when apply" - {
      "should have its size" in {
        val tree = RedBlackTree.empty[Long]
        tree.insert(5L)
        tree.size shouldBe 1L
      }
    }
    "when foreachBreadthFirst" - {
      "should apply `f` each values BF(Breadth First) order"in {
        val tree = RedBlackTree.empty[Long]
        println("inserting 1L")
        tree.insert(1L)
        tree.foreachBreadthFirst(println)
        println("-" * 10)

        println("inserting 2L")
        tree.insert(2L)
        tree.foreachBreadthFirst(println)
        println("-" * 10)

        println("inserting 3L")
        tree.insert(3L)
        tree.foreachBreadthFirst(println)
        println("-" * 10)

        println("inserting 4L")
        tree.insert(4L)
        tree.foreachBreadthFirst(println)
        println("-" * 10)
      }
    }
  }

}
