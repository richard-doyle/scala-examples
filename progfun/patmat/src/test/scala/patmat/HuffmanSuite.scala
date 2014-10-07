package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val l1 = Leaf('a', 2)
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("weight of a leaf") {
    new TestTrees {
      assert(weight(l1) === 2)
    }
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }
  
  test("chars of a leaf") {
    new TestTrees {
      assert(chars(l1) === List('a'))
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }
  
  test("times list chars") {
    val charList = List('a', 'b', 'c', 'a')
    assert(times(charList) === List(('a', 2), ('b', 1), ('c', 1)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }
  
  test("singleton for single tree") {
    new TestTrees {
      assert(singleton(List(t1)))
    }
  }
  
  test("singleton for multiple trees") {
    new TestTrees {
      assert(!singleton(List(t1, t2)))
    }
  }
  
  test("singleton for no trees") {
    new TestTrees {
      assert(!singleton(List()))
    }
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("combine of some leaf list changing order") {
    val leaflist = List(Leaf('e', 2), Leaf('t', 3), Leaf('x', 4))
    assert(combine(leaflist) === List(Leaf('x',4), Fork(Leaf('e',2),Leaf('t',3),List('e', 't'),5)))
  }

  test("combine of some short leaf list") {
    val leaflist = List(Leaf('e', 2))
    assert(combine(leaflist) === List(Leaf('e', 2)))
  }

  test("until of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(until(singleton, combine)(leaflist) === List(Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4), List('e', 't', 'x'), 7)))
  }

  test("createCodeTree of some chars") {
    val chars = List('e', 'x', 't', 'x', 't', 'x', 'x')
    assert(createCodeTree(chars) === Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4), List('e', 't', 'x'), 7))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
  
  test("codeBits should return list of bits") {
    val codeTable = List(('a', List(0, 1, 0)), ('b', List(1, 1, 1)))
    assert(codeBits(codeTable)('a') === List(0, 1, 0))
    assert(codeBits(codeTable)('b') === List(1, 1, 1))
  }
  
  test("convert should return a code table") {
    new TestTrees {
      assert(convert(t2) === List(('a', List(0, 0)), ('b', List(0, 1)), ('d', List(1))))
    }
  }
  
  test("mergeCodeTables should add 0 to all elements in first arg, 1 to second and concat") {
    val codeTable1 = List(('a', List(1, 0)), ('b', List(0, 0)))
    val codeTable2 = List(('c', List(1, 1)), ('d', List(0, 1)))
    assert(mergeCodeTables(codeTable1, codeTable2) === List(('a', List(0, 1, 0)), ('b', List(0, 0, 0)), ('c', List(1, 1, 1)), ('d', List(1, 0, 1))))
  }
  
  test("decode and quickEncode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }
}
