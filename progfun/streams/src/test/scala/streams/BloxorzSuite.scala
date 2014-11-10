package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(!terrain(Pos(4,11)), "4,11")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }

  test("isStanding") {
    new Level1 {
      val b1 = Block(Pos(0,0), Pos(0,0))
      val b2 = Block(Pos(0,1), Pos(0,2))
      assert(b1.isStanding, "standing true")
      assert(!b2.isStanding, "standing false")
    }
  }

  test("isLegal") {
    new Level1 {
      val b1 = Block(Pos(0,0), Pos(0,0))
      val b2 = Block(Pos(0,0), Pos(0,1))
      val b3 = Block(Pos(0,7), Pos(0,7))
      val b4 = Block(Pos(0,7), Pos(0,8))
      val b5 = Block(Pos(0,2), Pos(0,3))
      assert(b1.isLegal, "legal 'standing' on")
      assert(b2.isLegal, "legal 'not standing' on")
      assert(!b3.isLegal, "legal 'standing' off")
      assert(!b4.isLegal, "legal 'not standing' off")
      assert(!b5.isLegal, "legal 'not standing' half on")
    }
  }

  test("startBlock") {
    new Level1 {
      assert(startBlock.b1 == Pos(1, 1), "start block")
      assert(startBlock.b1 == Pos(1, 1), "start block")
    }
  }
  
  test("neighbors") {
    new Level1 {
      val b1 = Block(Pos(3,3), Pos(3,3))
      assert(b1.neighbors.toSet ==
        List(
            (Block(Pos(3,1), Pos(3,2)), Left),
            (Block(Pos(3,4), Pos(3,5)), Right),
            (Block(Pos(1,3), Pos(2,3)), Up),
            (Block(Pos(4,3), Pos(5,3)), Down)).toSet)
    }
  }
  
  test("legalNeighbors") {
    new Level1 {
      val b1 = Block(Pos(3,3), Pos(3,3))
      assert(b1.legalNeighbors.toSet ==
        List(
            (Block(Pos(3,1), Pos(3,2)), Left),
            (Block(Pos(3,4), Pos(3,5)), Right),
            (Block(Pos(1,3), Pos(2,3)), Up)).toSet, "legal neighbors")
    }
  }
  
  test("done") {
    new Level1 {
	  val b1 = Block(Pos(4,7), Pos(4,7))
	  val b2 = Block(Pos(4,7), Pos(4,8))
	  val b3 = Block(Pos(2,3), Pos(2,3))
	  assert(done(b1), "standing on target")
	  assert(!done(b2), "not standing on target")
	  assert(!done(b3), "standing off target")
    }
  }
  
  test("neighborsWithHistory") {
    new Level1 {
      val b1 = Block(Pos(3,3), Pos(3, 3))
      val history = List(Left, Up, Down)
      val result = neighborsWithHistory(b1, history)
      assert(result.toSet ==
        List(
            (Block(Pos(3,1), Pos(3,2)), List(Left, Left, Up, Down)),
            (Block(Pos(3,4), Pos(3,5)), List(Right, Left, Up, Down)),
            (Block(Pos(1,3), Pos(2,3)), List(Up, Left, Up, Down))
        ).toSet, "legal neighbors with history")
    }
  }
  
  test("neighborsWithHistory again") {
    new Level1 {
      val b1 = Block(Pos(1,1), Pos(1, 1))
      val history = List(Left, Up)
      assert(neighborsWithHistory(b1, history).toSet ==
        List(
            (Block(Pos(1,2), Pos(1,3)), List(Right, Left, Up)),
            (Block(Pos(2,1), Pos(3,1)), List(Down, Left, Up))
        ).toSet, "legal neighbors with history")
    }
  }
  
  test("newNeighborsOnly") {
    new Level1 {
      val neighbors = Set(
		  			    (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
		  			    (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))).toStream
	  val explored = Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))
	  
      assert(newNeighborsOnly(neighbors, explored).toSet ==
        Set((Block(Pos(2,1), Pos(3,1)), List(Down, Left, Up))), "new neighbors only")
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }
}