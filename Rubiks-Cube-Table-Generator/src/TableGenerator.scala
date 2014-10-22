import common._
import java.io._

/**
 * Entry point for generating binary files. These files will represent every permutation
 * (of position and orientation) for three groups of cubies: the 8 corners, 6 of the sides,
 * and the other 6 sides. The files will assume a certain order for these permutations,
 * and will rely on this understood order so that we don't have to store the state itself,
 * but only the number of moves it takes to get that state back to the goal state. We use
 * the hashing function to figure out this order.
 *
 * As per Korf's suggestion, a turn for a face consists of one of three options: a clockwise
 * 90-degree turn, a counterclockwise 90-degree turn, or a 180-degree turn. We use a Short to
 * represent these options, using the values 1, 2, and 3. But note that while a 2 means
 * 180 degrees, it does not consist of two separate 90-degree turns. This is to avoid duplicate
 * search tree nodes.
 *
 * @author John Paul Welsh
 */
object TableGenerator {

  /**
   * Global variables to represent the corner and side cubies in an unchanged solved cube.
   * These are instantiated in main and then used in the hash functions.
   */
  var solvedCube: Cube = setSolvedCube()
  var solvedCornerCubies: Array[Cubie] = getCubeCornerList(solvedCube)
  var solvedSideCubies: Array[Cubie] = getCubeSideList(solvedCube)

  /**
   * Determines whether two cubies match, based solely on contents and not order.
   *
   * @param input  The Cubie in question
   * @param solved   The Cubie from the solved state
   * @return       True if the Cubies match (regardless of orientation), false otherwise
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



  def createStates(solved: Cube) {

    var curr = setSolvedCube()

    for (u <- 0 to 1) {
      curr = turn_U(solvedCube, u)
      for (d <- 0 to 1) {
        curr = turn_D(solvedCube, d)
        println(hashCorners(getCubeCornerList(curr)))
      }
//      hash(curr)
      println(hashCorners(getCubeCornerList(curr)))
    }

  }

  def factorial(n: Int): Int = n match {
    case 0 => 1
    case _ => n * factorial(n-1)
  }

  def hashCorners(cubies: Array[Cubie]): Int = {
    var values = Array(0, 1, 2, 3, 4, 5, 6, 7)

    def accumulate(cbs: Array[Int], fact: Int, accum: Int): Int = {
      if (fact < 0) accum
      else {
        // the value between 0 to length-1 that this # in cbs currently has (after shifting)
        val idx = values.indexOf(cbs.head)
        // the math, not including the orientation math
        val newAccum = factorial(fact) * idx
        // remove idx from values and shift everything down
        values = values.take(idx) ++ values.drop(idx + 1)

        accumulate(cbs.tail, fact-1, accum + newAccum)
      }
    }

    //val solvedIndices = getCubeCornerList(setSolvedCube())
    val cornerIndices = getCubieIndices(cubies, solvedCornerCubies, Array.ofDim(8))

    accumulate(cornerIndices, 7, 0)
  }

  def hashSides1(cubies: Array[Cubie]): Int = {
    val sidesVals = Array(5, 4, 3, 2, 1, 0)

    0
  }

  def hashSides2(cubies: Array[Cubie]): Int = {
    val sidesVals = Array(5, 4, 3, 2, 1, 0)

    0
  }

  def hashAndWrite(c: Cube) {
    writeToFile(hashCorners(getCubeCornerList(c)))
    writeToFile(hashSides1(getFirstHalfOfCubeSides(c)))
    writeToFile(hashSides2(getSecondHalfOfCubeSides(c)))
  }


  def writeToFile(state: Int) {

  }

  def main(args: Array[String]) {
//    createStates(solvedCube)

    val out: DataOutputStream = new DataOutputStream(new FileOutputStream("text1.txt"))
    out.writeBytes("yo: " + 3)
    out.close()
  }
}