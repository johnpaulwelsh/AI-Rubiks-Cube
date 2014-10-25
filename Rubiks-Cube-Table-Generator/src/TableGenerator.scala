import common._
import java.io.{DataOutputStream, FileOutputStream, ObjectOutputStream}
import scala.collection.mutable.HashMap

/**
 * Entry point for generating binary files. These files will represent every permutation
 * (of position and orientation) for three groups of cubies: the 8 corners, 6 of the sides,
 * and the other 6 sides. The files will assume a certain order for these permutations,
 * and will rely on this understood order so that we don't have to store the state itself,
 * but only the number of moves it takes to get that state back to the goal state. We use
 * the hashing function to figure out this order, because it gives us a "position" number
 * for each state.
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

  /*
   * Global variables to represent the corner and side cubies in an unchanged solved cube.
   * These are instantiated in main and then used in the hash functions.
   */
  val solvedCube: Cube = setSolvedCube()
  var solvedCornerCubies: Array[Cubie] = getCubeCornerList(solvedCube)
  var solvedFirstSideCubies: Array[Cubie] = getFirstHalfOfCubeSides(solvedCube)
  var solvedSecondSideCubies: Array[Cubie] = getSecondHalfOfCubeSides(solvedCube)

  /*
   * Global mutable HashMaps, where the key is the result of passing a cube state to the hashing
   * function, and the value is the number of moves it took to get from the solved cube to that
   * state. One for corners, and two for sides.
   */
  val cornerHash, side1Hash, side2Hash = HashMap[Int, Int]()

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
   * @param cubies  the cubies in question, used to determine what constitutes a cube state
   * @return  a hash value for this state
   */
  def doHashCorners(cubies: Array[Cubie]): Int = {
    var cornerVals = Array(0, 1, 2, 3, 4, 5, 6, 7)

    def accumulate(cbs: Array[Int], fact: Int, accum: Int): Int = {
      if (fact < 0) accum
      else {
        // the value between 0 to length-1 that this # in cbs currently has (after shifting)
        val idx = cornerVals.indexOf(cbs.head)
        // the math, not including the orientation math
        val newAccum = factorial(fact) * idx
        // remove idx from values and shift everything down
        cornerVals = cornerVals.take(idx) ++ cornerVals.drop(idx + 1)

        accumulate(cbs.tail, fact-1, accum + newAccum)
      }
    }

    val cornerIndices = getCubieIndices(cubies, solvedCornerCubies, Array.ofDim(8))
    accumulate(cornerIndices, 7, 0)
  }

  /**
   * Hashing function for side cubies. Generates a unique Integer to represent the state of the cube,
   * in regards to the cubies being looked at.
   *
   * @param cubies          the cubies in question, used to determine what constitutes a cube state
   * @param isFirst6Cubies  determines which half of the side cubies we are working with
   * @return  a hash value for this state
   */
  def doHashSides(cubies: Array[Cubie], isFirst6Cubies: Boolean): Int = {
    var sideVals = Array(0, 1, 2, 3, 4, 5)

    // divide the whole thing by 6!

    def accumulate(cbs: Array[Int], fact: Int, accum: Int): Int = {
      if (fact < 0) accum
      else {
        // the value between 0 to length-1 that this # in cbs currently has (after shifting)
        val idx = sideVals.indexOf(cbs.head)
        // the math, not including the orientation math
        val newAccum = factorial(fact) * idx
        // remove idx from values and shift everything down
        sideVals = sideVals.take(idx) ++ sideVals.drop(idx + 1)

        accumulate(cbs.tail, fact-1, accum + newAccum)
      }
    }

    val sideIndices = {
      if (isFirst6Cubies) getCubieIndices(cubies, solvedFirstSideCubies, Array.ofDim(6))
      else                getCubieIndices(cubies, solvedSecondSideCubies, Array.ofDim(6))
    }
    accumulate(sideIndices, 5, 0)
  }

  /**
   * Creates a breadth-first tree search for all move combinations upon a Rubik's Cube.
   * Stops when a branch of the tree reaches a repeated hash value.
   *
   * @param c  a representation of a Rubik's Cube
   */
  def createStates(c: Cube, count: Int) {

    def recurse() {
      createStates(turn_U(c, 1), count+1)
      createStates(turn_D(c, 1), count+1)
      createStates(turn_L(c, 1), count+1)
      createStates(turn_R(c, 1), count+1)
      createStates(turn_F(c, 1), count+1)
      createStates(turn_B(c, 1), count+1)

      createStates(turn_U(c, 2), count+1)
      createStates(turn_D(c, 2), count+1)
      createStates(turn_L(c, 2), count+1)
      createStates(turn_R(c, 2), count+1)
      createStates(turn_F(c, 2), count+1)
      createStates(turn_B(c, 2), count+1)

      createStates(turn_U(c, 3), count+1)
      createStates(turn_D(c, 3), count+1)
      createStates(turn_L(c, 3), count+1)
      createStates(turn_R(c, 3), count+1)
      createStates(turn_F(c, 3), count+1)
      createStates(turn_B(c, 3), count+1)
    }

    val turnedCube = turn_U(c, 1)
    val turnedCubeCorners = getCubeCornerList(turnedCube)
    val hc  = doHashCorners(turnedCubeCorners)
    println(hc)

    if (count <= 0) recurse()

    /*
    val hs1 = doHashSides(getFirstHalfOfCubeSides(c), true)
    val hs2 = doHashSides(getSecondHalfOfCubeSides(c), false)

    if (!cornerHash.contains(hc)) {
      cornerHash.put(hc, count)
      recurse()
    }

    if (!side1Hash.contains(hs1)) {
      side1Hash.put(hs1, count)
      recurse()
    }

    if (!side2Hash.contains(hs2)) {
      side2Hash.put(hs2, count)
      recurse()
    }
    */
  }

  /**
   * Writes each of the HashMaps to a binary file.
   */
  def writeToFile() {
    var out = new ObjectOutputStream(new FileOutputStream("cornertable.bin"))
    out.writeObject(cornerHash.values)
    out.close()

    out = new ObjectOutputStream(new FileOutputStream("sidetable1.bin"))
    out.writeObject(side1Hash.values)
    out.close()

    out = new ObjectOutputStream(new FileOutputStream("sidetable2.bin"))
    out.writeObject(side2Hash.values)
    out.close()
  }

  def main(args: Array[String]) {
    createStates(solvedCube, 0)
    //writeToFile()
  }
}