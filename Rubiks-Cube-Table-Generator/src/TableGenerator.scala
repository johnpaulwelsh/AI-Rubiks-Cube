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

  def main(args: Array[String]) {
    println("Dingo dongo")
  }
}
