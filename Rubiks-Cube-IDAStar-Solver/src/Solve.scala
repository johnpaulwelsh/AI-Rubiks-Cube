import java.io.{BufferedInputStream, FileInputStream}

import common._
import scala.io.Source
import scala.util.{Try, Failure, Success}

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

  var turnCube: Cube = null

  /**
   * Determines whether two cubies match, based solely on contents and not order.
   *
   * @param input   The Cubie in question
   * @param solved  The Cubie from the solved state
   * @return  true if the Cubies match (regardless of orientation), false otherwise
   */
  def matchingCubies(input: Cubie, solved: Cubie): Boolean = {
    if (solved.isEmpty) true
    else if (!input.deep.contains(solved.head)) false
    else matchingCubies(input, solved.tail)
  }

  /**
   * Maps the location that a cubie is (in seqNow) to the location
   * it ought to be (gotten from seqSolved).
   *
   * @param seqNow     the sequence of cubies from the input
   * @param seqSolved  the sequence of cubies in the solved state
   * @param accum      an array showing, for each element in seqNow, what element
   *                   it belongs to in the solved state
   * @return  the accumulated array of position indicies
   */
  def getCubieIndices(seqNow: Array[Cubie], seqSolved: Array[Cubie], accum: Array[Int]): Array[Int] = {
    for (i <- 0 until seqSolved.length) {
      for (j <- 0 until seqNow.length) {
        if (matchingCubies(seqSolved(i), seqNow(j))) accum(j) = i
      }
    }
    accum
  }

  /**
   * Hashing function for corner cubies. Generates a unique Integer to represent the state of the cube,
   * in regards to the cubies being looked at.
   *
   * @param cubiesList  the cubies in question, used to determine what constitutes a cube state
   * @return  a hash value for this state
   */
  def doHashCorners(cubiesList: Array[Cubie]): Int = {
    var cornerVals = (0 to 7).toArray

    def accumulate(actualCubies: Array[Cubie], crnrIndices: Array[Int], fact: Int, accum: Int): Int = {
      if (fact < 0) accum
      else {
        // the value between 0 to length-1 that this # in cbs currently has (after shifting)
        val idx = cornerVals.indexOf(crnrIndices.head)
        // Can be 0, 1, or 2  (actualCubies is getting smaller each time, cubiesList is not)
        val orient = getCornerParity(actualCubies.head, cubiesList)
        // The math! --- (fact! * "points") + ( 3^(fact-1) * orient * 8! )
        val newAccum = {
          if (fact > 0) (factorial(fact) * idx) + (Math.pow(3, fact-1).toInt * orient * factorial(8))
          else factorial(fact)
        }
        // Remove idx from values and shift everything down
        cornerVals = cornerVals.take(idx) ++ cornerVals.drop(idx+1)

        accumulate(actualCubies.tail, crnrIndices.tail, fact-1, accum + newAccum)
      }
    }

    val cornerIndices = getCubieIndices(cubiesList, solvedCornerCubies, Array.ofDim(8))
    accumulate(cubiesList, cornerIndices, 7, 0)
  }

  /**
   * Hashing function for side cubies. Generates a unique Integer to represent the state of the cube,
   * in regards to the cubies being looked at.
   *
   * @param cubiesList      the cubies in question, used to determine what constitutes a cube state
   * @param isFirst6Cubies  determines which half of the side cubies we are working with
   * @return  a hash value for this state
   */
  def doHashSides(cubiesList: Array[Cubie], isFirst6Cubies: Boolean): Int = {
    var sideVals = (0 to 11).toArray

    def accumulate(cbs: Array[Int], fact: Int, accum: Int): Int = {
      if (fact < 6) accum
      else {
        // the value between 0 to length-1 that this # in cbs currently has (after shifting)
        val idx = sideVals.indexOf(cbs.head)
        // Can be 0 or 1
        val orient = getSideParity(cubiesList.head)
        // The math! --- ( (fact! * "points") / 6! ) + ( 2^(fact-6) * orient * 12!/6! )
        val newAccum = ( (factorial(fact) * idx) / factorial(6) ) + (Math.pow(2, fact-6).toInt * orient * (factorial(12)/factorial(6))  )
        // remove idx from values and shift everything down
        sideVals = sideVals.take(idx) ++ sideVals.drop(idx+1)

        accumulate(cbs.tail, fact-1, accum + newAccum)
      }
    }

    val sideIndices = {
      if (isFirst6Cubies) getCubieIndices(cubiesList, solvedFirstSideCubies, Array.ofDim(6))
      else getCubieIndices(cubiesList, solvedSecondSideCubies, Array.ofDim(6))
    }
    // Must divide by 6! at the end!
    accumulate(sideIndices, 11, 0)
  }

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
    //    val bis: java.io.BufferedInputStream = new BufferedInputStream(new FileInputStream(table))
    //    val bsArray = Stream.continually(bis.read).takeWhile(-1 !=).map(_.toByte).toArray
    //
    //    val pair = bsArray(hashValue/2)
    //    if (hashValue % 2 == 0) (pair >>> 4).toByte
    //    else                    (pair & 0x0F).toByte
    0.toByte
  }

  /**
   * Function to calculate the h(n), which is the maximum of the three turn counts
   * given back from the heuristic tables.
   *
   * @param node  the cube we are looking at
   * @return  the maximum number of moves gotten from the tables
   */
  def calculateHeuristic(node: Cube): Byte = {
    val hc  = doHashCorners(getCubeCornerList(node)) - 1
    val hs1 = doHashSides(getFirstHalfOfCubeSides(node), true) - 1
    val hs2 = doHashSides(getFirstHalfOfCubeSides(node), false) - 1
    val cornerHeur = readFromHeuristicTable(hc,  "cornertable")
    val sides1Heur = readFromHeuristicTable(hs1, "sidetable1")
    val sides2Heur = readFromHeuristicTable(hs2, "sidetable2")

    List(cornerHeur, sides1Heur, sides2Heur).reduceLeft(_ max _)
  }

  /**
   * The A* part of the IDA* search. We expand nodes in relation to their calculated f(n),
   * which takes into account the step cost to arrive where we are and the heuristic
   * estimate from each child node to the goal.
   *
   * @param node      the current state of the cube
   * @param stepCost  the number of moves taken to get to this point
   * @param cutoff    the number of moves at which we will stop searching for a solution
   * @param output    the final output, to which we will add the next move we choose to make
   * @return  the output for this search (either a solution path or a blank string)
   */
  def search(node: Cube, stepCost: Int, cutoff: Int, output: String): String = {

    if (isGoal(node))     output
    else if (cutoff <= 0) "None"
    else {
      // Get the list of successor nodes
      val successors = getSuccessors(node)

      // Find all of the successors with the lowest heuristic value
      var lowestHeur = 12
      for (succ <- successors) {
        val heur = succ.heuristic
        if (heur < lowestHeur) lowestHeur = heur
      }

      val bestSuccessors = successors.filter(x => x.heuristic == lowestHeur)

      // Find the lowest heuristic by going through the array
      // Filter the array to only include the ones with that heuristic value

      var result = "None"
      var idx = 0
      while (idx < bestSuccessors.length) {
        if (result == "None") {
          val succ = bestSuccessors(idx)
          val currResult = search(succ.state, stepCost+1, cutoff-1, output+succ.fullMove)
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
      else                  loop(d+1, search(initial, 0, d, ""))
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

    //val filename = "../../countstates/cube04"
    val filename = args(0)
    attemptToReadFile(filename) match {
      case Failure(f)     => println("No file found.")
      case Success(lines) =>
        val cube = arrangeInput(lines)
        val isValid = CubeValidator.isValidCube(lines, cube, solvedCube)

        if (lines.isEmpty) println("Empty file.")
        else if (!isValid) println("Invalid cube.")
        else               println(iterativeDeepening(cube, 15))
    }
  }
}
