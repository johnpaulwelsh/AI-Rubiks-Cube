/**
 * An Enumerated type to represent a move on a Rubik's Cube.
 * For determining what the last move was in the search tree.
 *
 * @author  John Paul Welsh
 */
object MoveEnum extends Enumeration {
  type Move = Value
  val NONE  = Value(-1)
  val UP    = Value(0)
  val DOWN  = Value(1)
  val LEFT  = Value(2)
  val RIGHT = Value(3)
  val FRONT = Value(4)
  val BACK  = Value(5)
}