package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val t4 = makeCodeTree(makeCodeTree(Leaf('x', 1), Leaf('e', 1)), Leaf('t', 2))
    val t5 = createCodeTree(List('a', 'b', 'c', 'c', 'c', 'c', 'c', 'b'))

    //until
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val res = until(singleton, combine)(leaflist)
    val t = res head
	}

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("weight of t2") {
    new TestTrees {
      assert(weight(t2) === 9)
    }
  }

  test("weight of t4") {
    new TestTrees {
      assert(weight(t4) === 4)
    }
  }

  test("weight of t5") {
    new TestTrees {
      assert(weight(t5) === 8)
    }
  }

  test("chars of t1") {
    new TestTrees {
      assert(chars(t1) === List('a', 'b'))
    }
  }

  test("chars of t2") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("chars of t4") {
    new TestTrees {
      assert(chars(t4) === List('x', 'e', 't'))
    }
  }

  test("chars of t5") {
    new TestTrees {
      assert(chars(t5) === List('a', 'b', 'c'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("singleton for a singleton") {
    assert(singleton(List[CodeTree](Leaf('e', 1))))
  }

  test("singleton for NOT a singleton") {
    assert(!singleton(List[CodeTree](Leaf('e', 1), Leaf('x', 2))))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("until of res resulted in singleton") {
    new TestTrees {
      assert(singleton(res))
    }
  }

  test("until of res resulted in weight 7") {
    new TestTrees {
      assert(weight(t) === 7)
    }
  }

  test("chars of t") {
    new TestTrees {
      assert(chars(t) === List('e', 't', 'x'))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

}
