import common._

class CubeState(var state: Cube, val fromTurn: String, val fromNumber: Int) {

  val fullMove: String = fromTurn + fromNumber
  val heuristic: Byte = Solve.calculateHeuristic(state)
}
