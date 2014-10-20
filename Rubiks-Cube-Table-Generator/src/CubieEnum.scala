/**
 * An Enumeration to represent a Cubie.
 */
object CubieEnum extends Enumeration {

  type CubieEnum = common.Cubie

  val GRW = Array('G', 'R', 'W')
  val BRW = Array('B', 'R', 'W')
  val GRY = Array('G', 'R', 'Y')
  val BRY = Array('B', 'R', 'Y')
  val GOW = Array('G', 'O', 'W')
  val BOW = Array('B', 'O', 'W')
  val GOY = Array('G', 'O', 'Y')
  val BOY = Array('B', 'O', 'Y')

  val RW = Array('x', 'R', 'W')
  val GR = Array('G', 'R', 'x')
  val BR = Array('B', 'R', 'x')
  val RY = Array('x', 'R', 'W')
  val GW = Array('G', 'x', 'W')
  val BW = Array('B', 'x', 'W')
  val GY = Array('G', 'x', 'Y')
  val BY = Array('B', 'x', 'Y')
  val OW = Array('x', 'O', 'W')
  val GO = Array('G', 'O', 'x')
  val BO = Array('B', 'O', 'x')
  val OY = Array('x', 'O', 'Y')
}