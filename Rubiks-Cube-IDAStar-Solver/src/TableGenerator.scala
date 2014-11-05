import common._
import MoveEnum._
import java.io.{DataOutputStream, FileOutputStream}

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
 * The hashing function is intellectual property of John Sullivan, with assistance by Daniel DeNinno.
 *
 * @author John Paul Welsh
 */
object TableGenerator {

  /*
   * 11 for corners, 10 for sides.
   */
  val MAX_MOVE_COUNT = 7

  val BIG_FACTORIAL: Int = 665280 // factorial(12)/factorial(6)

  /*
   * Global variables to represent the number of elements the corner table and side table
   * are expected to have, according to Korf's paper.
   */
  val MAX_CORNER_SIZE = 88179840
  val MAX_SIDE_SIZE   = 42577420 + 2

  /*
   * Global variables to represent a solved version of the cube, the corner and
   * side cubies in an unchanged solved cube.
   */
  val solvedCube: Cube = setSolvedCube()
  var solvedCornerCubies: Array[Cubie] = getCubeCornerList(solvedCube)
  var solvedSideCubies: Array[Cubie] = getCubeSideList(solvedCube)

  /*
   * Global arrays to hold the turn numbers that are obtained from each hashed cube state.
   * each array is initialized at the exact size they ought to be by the end (8!*3^7 for corners,
   * 12!/6!*2^6 for sides), and are populated with the number 12 to start with. This is because
   * no move count should be 12 or higher, so when we compare whether we have a value in a particular
   * element array, we can just see if the value we got this time is < 12. This also guarantees that
   * we hold onto the shortest move count to get to any state.
   */
  var cornerHashArray: Array[Byte] = Array.fill(MAX_CORNER_SIZE)(12)
//  val side1HashArray: Array[Byte]  = Array.fill(MAX_SIDE_SIZE)(12)
//  val side2HashArray: Array[Byte]  = Array.fill(MAX_SIDE_SIZE)(12)

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
  def getCubieIndicesForCorners(seqNow: Array[Cubie], seqSolved: Array[Cubie], accum: Array[Int]): Array[Int] = {
    for (i <- 0 until seqSolved.length) {
      for (j <- 0 until seqNow.length) {
        if (CubeValidator.matchingCubies(seqSolved(i), seqNow(j))) accum(j) = i
      }
    }
    accum
  }

  /**
   * Same as getCubieIndices (above), but specifically for side cubies
   *
   * @param isFirst6   true if this is for the first sides table, false if for the second
   * @param seqNow     the sequence of cubies from the input
   * @param seqSolved  the sequence of cubies in the solved state
   * @param accum      an array showing, for each element in seqNow, what element
   *                   it belongs to in the solved state
   * @return  the accumulated array of position indicies
   */
  def getCubieIndicesForSides(isFirst6: Boolean, seqNow: Array[Cubie], seqSolved: Array[Cubie], accum: Array[Int]): Array[Int] = {
    for (i <- 0 until seqSolved.length) {
      for (j <- 0 until seqNow.length) {
        if (CubeValidator.matchingCubies(seqSolved(i), seqNow(j))) accum(j) = i
      }
    }
    if (isFirst6) accum.take(6) else accum.drop(6)
  }

  /**
   * Hashing function for corner cubies. Generates a unique Integer to represent the state of the cube,
   * in regards to the cubies being looked at.
   *
   * @param cubiesList  the cubies in question, used to determine what constitutes a cube state
   * @return  a hash value for this state
   */
  def doHashCorners(cubiesList: Array[Cubie]): Int = {

    // TODO: Turns out, this isn't working either. It's close though.

    var cornerVals = (0 to 7).toArray

    def accumulate(actualCubies: Array[Cubie], crnrIndices: Array[Int], fact: Int, accum: Int): Int = {
      if (fact < 0) accum
      else {
        // the value between 0 to length-1 that this # in actualCubies currently has (after shifting)
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

    val cornerIndices = getCubieIndicesForCorners(cubiesList, solvedCornerCubies, Array.ofDim(8))
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

    // TODO: This isn't working.

    var sideVals = (0 to 11).toArray

    def accumulate(actualCubies: Array[Cubie], sideIndices: Array[Int], fact: Int, accum: Int): Int = {
      if (fact < 6) accum
      else {

        // the value between 0 to length-1 that this # in cbs currently has (after shifting)
        val idx = sideVals.indexOf(sideIndices.head)

        // Can be 0 or 1
        val orient = getSideParity(actualCubies.head)
        // The math! --- ( (fact! * "points") / 6! ) + ( 2^(fact-6) * orient * 12!/6! )
        val newAccum = ( (factorial(fact) * idx) / factorial(6) ) + (Math.pow(2, fact-6).toInt * orient * BIG_FACTORIAL )
        // remove idx from values and shift everything down
        sideVals = sideVals.take(idx) ++ sideVals.drop(idx+1)

        accumulate(actualCubies.tail, sideIndices.tail, fact-1, accum + newAccum)
      }
    }

    val sideIndices = getCubieIndicesForSides(isFirst6Cubies, cubiesList, solvedSideCubies, Array.ofDim(12))
    accumulate(cubiesList, sideIndices, 11, 0)
  }

  /**
   * Creates a breadth-first tree search for all move combinations upon a Rubik's Cube.
   * Stops when a branch of the tree reaches a repeated hash value.
   *
   * @param c  a representation of a Rubik's Cube
   */
  def createStates(c: Cube, count: Int, lastMove: Move) {

    val hc = doHashCorners(getCubeCornerList(c)) - 1
//    val hs1 = doHashSides(getFirstHalfOfCubeSides(c), true)
//    val hs2 = doHashSides(getSecondHalfOfCubeSides(c), false)

    if (count > MAX_MOVE_COUNT)            return
    else if (count >= cornerHashArray(hc)) return
    else cornerHashArray(hc) = count.toByte

//    if (count > MAX_MOVE_COUNT)            return
//    else if (count >= side1HashArray(hs1)) return
//    else side1HashArray(hs1) = count.toByte

//    if (count > MAX_MOVE_COUNT)            return
//    else if (count >= side2HashArray(hs2)) return
//    else side2HashArray(hs2) = count.toByte

    // UP, LEFT, and FRONT are primary
    lastMove match {
      case NONE =>
        createStates(turn_U(c, 1), count + 1, UP)
        createStates(turn_D(c, 1), count + 1, DOWN)
        createStates(turn_L(c, 1), count + 1, LEFT)
        createStates(turn_R(c, 1), count + 1, RIGHT)
        createStates(turn_F(c, 1), count + 1, FRONT)
        createStates(turn_B(c, 1), count + 1, BACK)

        createStates(turn_U(c, 2), count + 1, UP)
        createStates(turn_D(c, 2), count + 1, DOWN)
        createStates(turn_L(c, 2), count + 1, LEFT)
        createStates(turn_R(c, 2), count + 1, RIGHT)
        createStates(turn_F(c, 2), count + 1, FRONT)
        createStates(turn_B(c, 2), count + 1, BACK)

        createStates(turn_U(c, 3), count + 1, UP)
        createStates(turn_D(c, 3), count + 1, DOWN)
        createStates(turn_L(c, 3), count + 1, LEFT)
        createStates(turn_R(c, 3), count + 1, RIGHT)
        createStates(turn_F(c, 3), count + 1, FRONT)
        createStates(turn_B(c, 3), count + 1, BACK)
        return

      case UP =>
        createStates(turn_D(c, 1), count + 1, DOWN)
        createStates(turn_L(c, 1), count + 1, LEFT)
        createStates(turn_R(c, 1), count + 1, RIGHT)
        createStates(turn_F(c, 1), count + 1, FRONT)
        createStates(turn_B(c, 1), count + 1, BACK)

        createStates(turn_D(c, 2), count + 1, DOWN)
        createStates(turn_L(c, 2), count + 1, LEFT)
        createStates(turn_R(c, 2), count + 1, RIGHT)
        createStates(turn_F(c, 2), count + 1, FRONT)
        createStates(turn_B(c, 2), count + 1, BACK)

        createStates(turn_D(c, 3), count + 1, DOWN)
        createStates(turn_L(c, 3), count + 1, LEFT)
        createStates(turn_R(c, 3), count + 1, RIGHT)
        createStates(turn_F(c, 3), count + 1, FRONT)
        createStates(turn_B(c, 3), count + 1, BACK)
        return

      case LEFT =>
        createStates(turn_U(c, 1), count + 1, UP)
        createStates(turn_D(c, 1), count + 1, DOWN)
        createStates(turn_R(c, 1), count + 1, RIGHT)
        createStates(turn_F(c, 1), count + 1, FRONT)
        createStates(turn_B(c, 1), count + 1, BACK)

        createStates(turn_U(c, 2), count + 1, UP)
        createStates(turn_D(c, 2), count + 1, DOWN)
        createStates(turn_R(c, 2), count + 1, RIGHT)
        createStates(turn_F(c, 2), count + 1, FRONT)
        createStates(turn_B(c, 2), count + 1, BACK)

        createStates(turn_U(c, 3), count + 1, UP)
        createStates(turn_D(c, 3), count + 1, DOWN)
        createStates(turn_R(c, 3), count + 1, RIGHT)
        createStates(turn_F(c, 3), count + 1, FRONT)
        createStates(turn_B(c, 3), count + 1, BACK)
        return

      case FRONT =>
        createStates(turn_U(c, 1), count + 1, UP)
        createStates(turn_D(c, 1), count + 1, DOWN)
        createStates(turn_L(c, 1), count + 1, LEFT)
        createStates(turn_R(c, 1), count + 1, RIGHT)
        createStates(turn_B(c, 1), count + 1, BACK)

        createStates(turn_U(c, 2), count + 1, UP)
        createStates(turn_D(c, 2), count + 1, DOWN)
        createStates(turn_L(c, 2), count + 1, LEFT)
        createStates(turn_R(c, 2), count + 1, RIGHT)
        createStates(turn_B(c, 2), count + 1, BACK)

        createStates(turn_U(c, 3), count + 1, UP)
        createStates(turn_D(c, 3), count + 1, DOWN)
        createStates(turn_L(c, 3), count + 1, LEFT)
        createStates(turn_R(c, 3), count + 1, RIGHT)
        createStates(turn_B(c, 3), count + 1, BACK)
        return

      case DOWN =>
        createStates(turn_L(c, 1), count + 1, LEFT)
        createStates(turn_R(c, 1), count + 1, RIGHT)
        createStates(turn_F(c, 1), count + 1, FRONT)
        createStates(turn_B(c, 1), count + 1, BACK)

        createStates(turn_L(c, 2), count + 1, LEFT)
        createStates(turn_R(c, 2), count + 1, RIGHT)
        createStates(turn_F(c, 2), count + 1, FRONT)
        createStates(turn_B(c, 2), count + 1, BACK)

        createStates(turn_L(c, 3), count + 1, LEFT)
        createStates(turn_R(c, 3), count + 1, RIGHT)
        createStates(turn_F(c, 3), count + 1, FRONT)
        createStates(turn_B(c, 3), count + 1, BACK)
        return

      case RIGHT =>
        createStates(turn_U(c, 1), count + 1, UP)
        createStates(turn_D(c, 1), count + 1, DOWN)
        createStates(turn_F(c, 1), count + 1, FRONT)
        createStates(turn_B(c, 1), count + 1, BACK)

        createStates(turn_U(c, 2), count + 1, UP)
        createStates(turn_D(c, 2), count + 1, DOWN)
        createStates(turn_F(c, 2), count + 1, FRONT)
        createStates(turn_B(c, 2), count + 1, BACK)

        createStates(turn_U(c, 3), count + 1, UP)
        createStates(turn_D(c, 3), count + 1, DOWN)
        createStates(turn_F(c, 3), count + 1, FRONT)
        createStates(turn_B(c, 3), count + 1, BACK)
        return

      case BACK =>
        createStates(turn_U(c, 1), count + 1, UP)
        createStates(turn_D(c, 1), count + 1, DOWN)
        createStates(turn_L(c, 1), count + 1, LEFT)
        createStates(turn_R(c, 1), count + 1, RIGHT)

        createStates(turn_U(c, 2), count + 1, UP)
        createStates(turn_D(c, 2), count + 1, DOWN)
        createStates(turn_L(c, 2), count + 1, LEFT)
        createStates(turn_R(c, 2), count + 1, RIGHT)

        createStates(turn_U(c, 3), count + 1, UP)
        createStates(turn_D(c, 3), count + 1, DOWN)
        createStates(turn_L(c, 3), count + 1, LEFT)
        createStates(turn_R(c, 3), count + 1, RIGHT)
        return
    }
  }

  /**
   * Writes each of the hash arrays to a binary file.
   */
  def writeToFile() {
    val out = new DataOutputStream(new FileOutputStream("cornertable"))
    val writtenArray: Array[Byte] = Array.ofDim(MAX_CORNER_SIZE/2)

    var writtenIdx = 0
    for (i <- 0 until MAX_CORNER_SIZE/2 by 2) {
      val first = cornerHashArray(i)
      val second = cornerHashArray(i+1)
      val combined = (first << 4) | (0xF & second)
      writtenArray(writtenIdx) = combined.toByte
      writtenIdx += 1
    }

    out.writeBytes(writtenArray.mkString)
  }

  def main(args: Array[String]) {
    createStates(solvedCube, 0, NONE)

    // Replace all turn counts that == 12 with 0's because our heuristic always needs to be admissible
    for (i <- 0 until MAX_SIDE_SIZE) {
      val currValue = cornerHashArray(i)
      cornerHashArray(i) = if (currValue == 12) 0.toByte else currValue
    }

    writeToFile()
  }
}