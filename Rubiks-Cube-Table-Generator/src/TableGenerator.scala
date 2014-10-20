import CubieEnum._
import common._

/**
 * Entry point for generating binary files. These files will represent every permutation
 * (of position and orientation) for three groups of cubies: the 8 corners, 6 of the sides,
 * and the other 6 sides. The files will assume a certain order for these permutations,
 * and will rely on this understood order so that we don't have to store the state itself,
 * but only the number of moves it takes to get that state back to the goal state.
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
   * Determines whether two cubies match, based solely on contents and not order.
   *
   * @param input  The Cubie in question
   * @param enum   An Enumerated type representing a Cubie (in the solved state)
   * @return       True if the Cubies match (regardless of orientation), false otherwise
   */
  def matchingCubies(input: Cubie, enum: CubieEnum): Boolean = {
    if (enum.isEmpty) true
    else if (!input.deep.contains(enum.head)) false
    else matchingCubies(input, enum.tail)
  }

  def findNewStateForSetOfCubies(cubies: Array[Cubie]) {

  }

  def representMoveInFile(state: Byte) {
    // Corners
    // 8! * 3^7 = 88179840 combinations
    // range of # of moves to get each corner to its goal = 0-11
    // 4 bits for each table entry

    // Edges (6 at a time)
    // 12!/6! * 2^6 = 42577920 combinations
    // range of # of moves to get each edge to its goal = 0-10
    // 4 bits for each table entry
  }

  def putMovesInFile(state: Byte) = {

  }

  def main(args: Array[String]) {
    val solvedCube = setSolvedCube()
//    val isCornerMatchList: List[Boolean] = getCubeCornerList(solvedCube).toList.map(matchingCubies(_, BOW))
    val perms = getCubeCornerList(solvedCube).toSeq.permutations
    println(perms.length)
  }
}