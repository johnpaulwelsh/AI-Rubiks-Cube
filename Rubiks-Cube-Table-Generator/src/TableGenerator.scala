import common._
import MoveEnum._
import java.io.{FileOutputStream, ObjectOutputStream}
import scala.collection.mutable

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
  val cornerHash, side1Hash, side2Hash = mutable.HashMap[Int, Int]()

  /*
   * Global value for the maximum number of moves that it can take to reach a new state of the cube
   * (for a subset of cubies, like just the corners, or just half of the sides).
   */

  var ZERO, ONE, TWO = 0
  var MAX_GENERATED_CORNER_KEY = 0

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

    def accumulate(cList: Array[Cubie], cbs: Array[Int], fact: Int, accum: Int): Int = {
      if (fact < 0) accum
      else {
        // the value between 0 to length-1 that this # in cbs currently has (after shifting)
        val idx = cornerVals.indexOf(cbs.head)
        // Can be 0, 1, or 2. cList is getting smaller each time, cubiesList is not
        val orient = getCornerParity(cList.head, cubiesList)


        if (orient == 0)      ZERO+=1
        else if (orient == 1) ONE+=1
        else if (orient == 2) TWO+=1


        // The math! --- (fact! * "points") + ( 3^(fact-1) * orient * 8! )
        val newAccum = {
          if (fact > 0) ( factorial(fact) * idx ) + ( Math.pow(3, fact-1).toInt * orient * factorial(8) )
          else          factorial(fact)
        }
        // Remove idx from values and shift everything down
        cornerVals = cornerVals.take(idx) ++ cornerVals.drop(idx+1)

        accumulate(cList.tail, cbs.tail, fact-1, accum + newAccum)
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
        // The math! --- (fact! * "points") + ( 2^(fact-1) * orient * 12!/6! )
        val newAccum = {
          if (fact > 0) ( factorial(fact) * idx ) + ( Math.pow(2, fact-1).toInt * orient * (factorial(12) / factorial(6)) )
          else          factorial(12)
        }
        // remove idx from values and shift everything down
        sideVals = sideVals.take(idx) ++ sideVals.drop(idx+1)

        accumulate(cbs.tail, fact-1, accum + newAccum)
      }
    }

    val sideIndices = {
      if (isFirst6Cubies) getCubieIndices(cubiesList, solvedFirstSideCubies, Array.ofDim(6))
      else                getCubieIndices(cubiesList, solvedSecondSideCubies, Array.ofDim(6))
    }
    // Must divide by 6! at the end!
    accumulate(sideIndices, 11, 0) / factorial(6)
  }

  /**
   * Creates a breadth-first tree search for all move combinations upon a Rubik's Cube.
   * Stops when a branch of the tree reaches a repeated hash value.
   *
   * @param c  a representation of a Rubik's Cube
   */
  def createStates(c: Cube, count: Int, lastMove: Move) {

    if (count >= 12) return

    val hc  = doHashCorners(getCubeCornerList(c))
    if (hc > MAX_GENERATED_CORNER_KEY) MAX_GENERATED_CORNER_KEY = hc

//    val hs1 = doHashSides(getFirstHalfOfCubeSides(c), true)
//    val hs2 = doHashSides(getSecondHalfOfCubeSides(c), false)

//    if (cornerHash.contains(hc) && side1Hash.contains(hs1) && side2Hash.contains(hs2)) return
//    else {
//      if (!cornerHash.contains(hc)) cornerHash.put(hc, count)
//      if (!side1Hash.contains(hs1)) side1Hash.put(hs1, count)
//      if (!side2Hash.contains(hs2)) side2Hash.put(hs2, count)
//    }

    if (cornerHash.contains(hc)) return
    else cornerHash.put(hc, count)

    // UP, LEFT, and FRONT are primary
    lastMove match {
      case NONE =>
        createStates(turn_U(c, 1), count+1, UP)
        createStates(turn_D(c, 1), count+1, DOWN)
        createStates(turn_L(c, 1), count+1, LEFT)
        createStates(turn_R(c, 1), count+1, RIGHT)
        createStates(turn_F(c, 1), count+1, FRONT)
        createStates(turn_B(c, 1), count+1, BACK)

        createStates(turn_U(c, 2), count+1, UP)
        createStates(turn_D(c, 2), count+1, DOWN)
        createStates(turn_L(c, 2), count+1, LEFT)
        createStates(turn_R(c, 2), count+1, RIGHT)
        createStates(turn_F(c, 2), count+1, FRONT)
        createStates(turn_B(c, 2), count+1, BACK)

        createStates(turn_U(c, 3), count+1, UP)
        createStates(turn_D(c, 3), count+1, DOWN)
        createStates(turn_L(c, 3), count+1, LEFT)
        createStates(turn_R(c, 3), count+1, RIGHT)
        createStates(turn_F(c, 3), count+1, FRONT)
        createStates(turn_B(c, 3), count+1, BACK)
        return

      case UP =>
        createStates(turn_D(c, 1), count+1, DOWN)
        createStates(turn_L(c, 1), count+1, LEFT)
        createStates(turn_R(c, 1), count+1, RIGHT)
        createStates(turn_F(c, 1), count+1, FRONT)
        createStates(turn_B(c, 1), count+1, BACK)

        createStates(turn_D(c, 2), count+1, DOWN)
        createStates(turn_L(c, 2), count+1, LEFT)
        createStates(turn_R(c, 2), count+1, RIGHT)
        createStates(turn_F(c, 2), count+1, FRONT)
        createStates(turn_B(c, 2), count+1, BACK)

        createStates(turn_D(c, 3), count+1, DOWN)
        createStates(turn_L(c, 3), count+1, LEFT)
        createStates(turn_R(c, 3), count+1, RIGHT)
        createStates(turn_F(c, 3), count+1, FRONT)
        createStates(turn_B(c, 3), count+1, BACK)
        return

      case LEFT =>
        createStates(turn_U(c, 1), count+1, UP)
        createStates(turn_D(c, 1), count+1, DOWN)
        createStates(turn_R(c, 1), count+1, RIGHT)
        createStates(turn_F(c, 1), count+1, FRONT)
        createStates(turn_B(c, 1), count+1, BACK)

        createStates(turn_U(c, 2), count+1, UP)
        createStates(turn_D(c, 2), count+1, DOWN)
        createStates(turn_R(c, 2), count+1, RIGHT)
        createStates(turn_F(c, 2), count+1, FRONT)
        createStates(turn_B(c, 2), count+1, BACK)

        createStates(turn_U(c, 3), count+1, UP)
        createStates(turn_D(c, 3), count+1, DOWN)
        createStates(turn_R(c, 3), count+1, RIGHT)
        createStates(turn_F(c, 3), count+1, FRONT)
        createStates(turn_B(c, 3), count+1, BACK)
        return

      case FRONT =>
        createStates(turn_U(c, 1), count+1, UP)
        createStates(turn_D(c, 1), count+1, DOWN)
        createStates(turn_L(c, 1), count+1, LEFT)
        createStates(turn_R(c, 1), count+1, RIGHT)
        createStates(turn_B(c, 1), count+1, BACK)

        createStates(turn_U(c, 2), count+1, UP)
        createStates(turn_D(c, 2), count+1, DOWN)
        createStates(turn_L(c, 2), count+1, LEFT)
        createStates(turn_R(c, 2), count+1, RIGHT)
        createStates(turn_B(c, 2), count+1, BACK)

        createStates(turn_U(c, 3), count+1, UP)
        createStates(turn_D(c, 3), count+1, DOWN)
        createStates(turn_L(c, 3), count+1, LEFT)
        createStates(turn_R(c, 3), count+1, RIGHT)
        createStates(turn_B(c, 3), count+1, BACK)
        return

      case DOWN =>
//        createStates(turn_U(c, 1), count+1, UP)
//        createStates(turn_D(c, 1), count+1, DOWN)
        createStates(turn_L(c, 1), count+1, LEFT)
        createStates(turn_R(c, 1), count+1, RIGHT)
        createStates(turn_F(c, 1), count+1, FRONT)
        createStates(turn_B(c, 1), count+1, BACK)

//        createStates(turn_U(c, 2), count+1, UP)
//        createStates(turn_D(c, 2), count+1, DOWN)
        createStates(turn_L(c, 2), count+1, LEFT)
        createStates(turn_R(c, 2), count+1, RIGHT)
        createStates(turn_F(c, 2), count+1, FRONT)
        createStates(turn_B(c, 2), count+1, BACK)

//        createStates(turn_U(c, 3), count+1, UP)
//        createStates(turn_D(c, 3), count+1, DOWN)
        createStates(turn_L(c, 3), count+1, LEFT)
        createStates(turn_R(c, 3), count+1, RIGHT)
        createStates(turn_F(c, 3), count+1, FRONT)
        createStates(turn_B(c, 3), count+1, BACK)
        return

      case RIGHT =>
        createStates(turn_U(c, 1), count+1, UP)
        createStates(turn_D(c, 1), count+1, DOWN)
//        createStates(turn_L(c, 1), count+1, LEFT)
//        createStates(turn_R(c, 1), count+1, RIGHT)
        createStates(turn_F(c, 1), count+1, FRONT)
        createStates(turn_B(c, 1), count+1, BACK)

        createStates(turn_U(c, 2), count+1, UP)
        createStates(turn_D(c, 2), count+1, DOWN)
//        createStates(turn_L(c, 2), count+1, LEFT)
//        createStates(turn_R(c, 2), count+1, RIGHT)
        createStates(turn_F(c, 2), count+1, FRONT)
        createStates(turn_B(c, 2), count+1, BACK)

        createStates(turn_U(c, 3), count+1, UP)
        createStates(turn_D(c, 3), count+1, DOWN)
//        createStates(turn_L(c, 3), count+1, LEFT)
//        createStates(turn_R(c, 3), count+1, RIGHT)
        createStates(turn_F(c, 3), count+1, FRONT)
        createStates(turn_B(c, 3), count+1, BACK)
        return

      case BACK =>
        createStates(turn_U(c, 1), count+1, UP)
        createStates(turn_D(c, 1), count+1, DOWN)
        createStates(turn_L(c, 1), count+1, LEFT)
        createStates(turn_R(c, 1), count+1, RIGHT)
//        createStates(turn_F(c, 1), count+1, FRONT)
//        createStates(turn_B(c, 1), count+1, BACK)

        createStates(turn_U(c, 2), count+1, UP)
        createStates(turn_D(c, 2), count+1, DOWN)
        createStates(turn_L(c, 2), count+1, LEFT)
        createStates(turn_R(c, 2), count+1, RIGHT)
//        createStates(turn_F(c, 2), count+1, FRONT)
//        createStates(turn_B(c, 2), count+1, BACK)

        createStates(turn_U(c, 3), count+1, UP)
        createStates(turn_D(c, 3), count+1, DOWN)
        createStates(turn_L(c, 3), count+1, LEFT)
        createStates(turn_R(c, 3), count+1, RIGHT)
//        createStates(turn_F(c, 3), count+1, FRONT)
//        createStates(turn_B(c, 3), count+1, BACK)
        return
    }
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
    createStates(solvedCube, 0, NONE)
    println(cornerHash.values.toList.count(_ == 0))
    println(cornerHash.values.toList.count(_ == 1))
    println(cornerHash.values.toList.count(_ == 2))
    println(cornerHash.values.toList.count(_ == 3))
    println(cornerHash.values.toList.count(_ == 4))
    println(cornerHash.values.toList.count(_ == 5))
    println(cornerHash.values.toList.count(_ == 6))
    println(cornerHash.values.toList.count(_ == 7))
    println(cornerHash.values.toList.count(_ == 8))
    println(cornerHash.values.toList.count(_ == 9))
    println(cornerHash.values.toList.count(_ == 10))
    println(cornerHash.values.toList.count(_ == 11))
//    println(cornerHash.size + ", " + cornerHash.values.toList.count(_ == 1))
//    println(MAX_GENERATED_CORNER_KEY)
    println(ZERO + ", " + ONE + ", " + TWO)
  }
}