import common._
import scala.io.Source
import scala.util.{Try, Failure, Success}
import java.io.RandomAccessFile

/**
 * Solver for a Rubik's Cube, using pre-generated tables and
 * Iterative Deepening A* Search to get the optimal path to the
 * solution.
 *
 * @author John Paul Welsh
 */
object Solve {

  val solvedCube: Cube = setSolvedCube()
  var solvedCornerCubies: Array[Cubie]     = getCubeCornerList(solvedCube)
  var solvedFirstSideCubies: Array[Cubie]  = getFirstHalfOfCubeSides(solvedCube)
  var solvedSecondSideCubies: Array[Cubie] = getSecondHalfOfCubeSides(solvedCube)

  /**
   * Goal test: determines whether the given state of the cube is solved.
   *
   * @param c  the state of the cube
   * @return  true if it matches the solved state, false otherwise
   */
  def isGoal(c: Cube): Boolean = c.deep sameElements solvedCube.deep

  /**
   * Function to get the successors of a node in the search. This means we make
   * all 18 kinds of moves to the current node, and put them all in an array.
   *
   * @param node  the current state of the cube
   * @return  an Array of all descendant nodes from the current one
   */
  def getSuccessors(node: Cube): Array[CubeState] = Array(
    new CubeState(turn_U(node, 1), "R", 1),
    new CubeState(turn_U(node, 2), "R", 2),
    new CubeState(turn_U(node, 3), "R", 3),

    new CubeState(turn_D(node, 1), "O", 1),
    new CubeState(turn_D(node, 2), "O", 2),
    new CubeState(turn_D(node, 3), "O", 3),

    new CubeState(turn_L(node, 1), "G", 1),
    new CubeState(turn_L(node, 2), "G", 2),
    new CubeState(turn_L(node, 3), "G", 3),

    new CubeState(turn_R(node, 1), "B", 1),
    new CubeState(turn_R(node, 2), "B", 2),
    new CubeState(turn_R(node, 3), "B", 3),

    new CubeState(turn_F(node, 1), "Y", 1),
    new CubeState(turn_F(node, 2), "Y", 2),
    new CubeState(turn_F(node, 3), "Y", 3),

    new CubeState(turn_B(node, 1), "W", 1),
    new CubeState(turn_B(node, 2), "W", 2),
    new CubeState(turn_B(node, 3), "W", 3)
  )

  /**
   * Function to read in a value from the given heuristic table.
   *
   * http://stackoverflow.com/questions/7598135/how-to-read-a-file-as-a-byte-array-in-scala
   * http://stackoverflow.com/questions/14533367/reading-4-bit-chunks-from-byte-array
   *
   * @param hashValue  the hash representing the current node's cube state
   * @param table  a selector for which table we are asking the answer from
   * @return  the turn count retrieved from hashValue's "position" in the table
   */
  def readFromHeuristicTable(hashValue: Int, table: String): Byte = {

    def attemptToReadHeuristicFile(file: String): Try[RandomAccessFile] = {
      Try(new RandomAccessFile(file, "r"))
    }

    attemptToReadHeuristicFile(table) match {
      case Failure(f)   => 0
      case Success(raf) =>
        raf.seek(hashValue/2)
        val pair = raf.readByte()
        if (hashValue % 2 == 0) (pair >>> 4).toByte
        else                    (pair & 0x0F).toByte
    }
  }

  /**
   * Function to calculate the h(n), which is the maximum of the three turn counts
   * given back from the heuristic tables.
   *
   * @param node  the cube we are looking at
   * @return  the maximum number of moves gotten from the tables
   */
  def calculateHeuristic(node: Cube): Byte = {
    val hc = TableGenerator.doHashCorners(getCubeCornerList(node)) - 1
    val cornerHeur = readFromHeuristicTable(hc,  "cornertable")

    //val hs1 = doHashSides(getFirstHalfOfCubeSides(node), true) - 1
    //val sides1Heur = readFromHeuristicTable(hs1, "sidetable1")
    val sides1Heur = 0.toByte

    //val hs2 = doHashSides(getFirstHalfOfCubeSides(node), false) - 1
    //val sides2Heur = readFromHeuristicTable(hs2, "sidetable2")
    val sides2Heur = 0.toByte

    List(cornerHeur, sides1Heur, sides2Heur).reduceLeft(_ max _)
  }

  /**
   * The A* part of the IDA* search. We expand nodes in relation to their calculated f(n),
   * which takes into account the step cost to arrive where we are and the heuristic
   * estimate from each child node to the goal.
   *
   * @param node      the current state of the cube
   * @param stepCost  the number of moves taken to get to this point
   * @param output    the final output, to which we will add the next move we choose to make
   * @return  the output for this search (either a solution path or a blank string)
   */
  def search(node: CubeState, stepCost: Int, maxDepth: Int, output: String): String = {

    if (isGoal(node.state)) output
    else if (stepCost > maxDepth) "None"
    else {
      // Get the list of successor nodes
      val successors = getSuccessors(node.state)

      var result = "None"
      var idx = 0
      while (idx < successors.length) {
        if (result == "None") {
          val succ = successors(idx)
          val currResult = search(succ, stepCost+1, maxDepth, output+succ.fullMove)
          if (currResult != "None") result = currResult
        }
        idx += 1
      }

      result
    }
  }

  /**
   * The outer piece of our IDA* search. This controls the depth to which we perform the A*,
   * thereby limiting the number of nodes we create.
   *
   * @param initial  the initial state of the cube
   * @param depth    the deepest number of moves we will use for our search
   * @return  the string containing the moves made in the solution path (or blank if not found)
   */
  def iterativeDeepening(initial: Cube, depth: Int): String = {

    def loop(d: Int, output: String): String = {
      if (output != "None") output
      else if (d > depth)   "None"
      else                  loop(d+1, search(new CubeState(initial, "x", 0), 0, d, ""))
    }

    loop(0, "None")
  }

  /**
   * The main function for this program. Reads input from a file, validates the
   * input as a solvable Rubik's Cube, and solves it using IDA*.
   *
   * @param args  command line arguments
   */
  def main(args: Array[String]) {

    def attemptToReadFile(filename: String): Try[Array[String]] = {
      Try(Source.fromFile(filename).getLines().map(x => x.trim()).toArray)
    }

    val filename = args(0)
    attemptToReadFile(filename) match {
      case Failure(f)     => println("No file found.")
      case Success(lines) =>
        val cube = arrangeInput(lines)
        val isValid = CubeValidator.isValidCube(lines, cube, solvedCube)

        if (lines.isEmpty) println("Empty file.")
        else if (!isValid) println("Invalid cube.")
        else               println(iterativeDeepening(cube, 20))
    }
  }
}