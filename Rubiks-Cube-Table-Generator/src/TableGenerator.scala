import common._

/**
 * Entry point for generating binary files. These files will represent every permutation
 * (of position and orientation) for three groups of cubies: the 8 corners, 6 of the sides,
 * and the other 6 sides. The files will assume a certain order for these permutations,
 * and will rely on this understood order so that we don't have to store the state itself,
 * but only the number of moves it takes to get that state back to the goal state.
 *
 * @author John Paul Welsh
 */

object TableGenerator {

  def findNewStateForSetOfCubies(cubies: Array[Cubie]) {

  }

  def representStateInFile(state: Cube) {
    // TODO: Figure out how to represent a state in the file, return it

    // Corners
    // 8! * 3^7 = 88179840 combinations
    // range of # of moves to get each corner to its goal = 0-11
    // 4 bits for each table entry

    // Edges (6 at a time)
    // 12!/6! * 2^6 = 42577920 combinations
    // range of # of moves to get each edge to its goal = 0-10
    // 4 bits for each table entry
  }

  def putStateInFile(state: Any) = {
    // TODO: Change type of state to whatever we're using to store a state
  }

  def main(args: Array[String]) {
    val solvedCube = setSolvedCube()
    println(solvedCube.deep)
  }
}