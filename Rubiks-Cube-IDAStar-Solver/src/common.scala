/**
 * Common utility functions that may be used at any time.
 * Can be imported into each file that wants to use them,
 * or called by common.function_name syntax.
 *
 * @author  John Paul Welsh
 */
object common {

  /*
   *
   * Type aliases for use in making a Rubik's Cube.
   *
   */

  type Cubie = Array[Char]
  type Cube = Array[Cubie]

  /*
   *
   * "Getter" and "setter" functions for Rubik's Cube.
   *
   */

  /**
   * Transforms a cross-shaped input into a cubie-divided cube.
   * The first element of a cubie is always the color resting on the x-axis,
   * and the second and third rest on the y- and z-axes, respectively.
   * If the cubie does not have a color on a certain axis, that spot is filled
   * with a lowercase 'x'.
   *
   * @param ls  an array of Strings, each one representing a row of the input
   * @return  a Cube object
   */
  def arrangeInput(ls: Array[String]): Cube = {
    val cube = Array.ofDim[Cubie](20)

    // corner cubies first, since they actually hold 3 colors    
    //               x                y                z
    cube(0)  = Array(ls(3).charAt(0), ls(0).charAt(0), ls(11).charAt(0))
    cube(2)  = Array(ls(3).charAt(8), ls(0).charAt(2), ls(11).charAt(2))
    cube(5)  = Array(ls(3).charAt(2), ls(2).charAt(0), ls(3).charAt(3))
    cube(7)  = Array(ls(3).charAt(6), ls(2).charAt(2), ls(3).charAt(5))

    cube(12) = Array(ls(5).charAt(0), ls(8).charAt(0), ls(9).charAt(0))
    cube(14) = Array(ls(5).charAt(8), ls(8).charAt(2), ls(9).charAt(2))
    cube(17) = Array(ls(5).charAt(2), ls(6).charAt(0), ls(5).charAt(3))
    cube(19) = Array(ls(5).charAt(6), ls(6).charAt(2), ls(5).charAt(5))

    // side cubies (with x's in the nonexistent spots)
    //              x                 y                z
    cube(1) = Array('x',             ls(0).charAt(1), ls(11).charAt(1))
    cube(3) = Array(ls(3).charAt(1), ls(1).charAt(0), 'x')
    cube(4) = Array(ls(3).charAt(7), ls(1).charAt(2), 'x')
    cube(6) = Array('x',             ls(2).charAt(1), ls(3).charAt(4))

    cube(8)  = Array(ls(4).charAt(0), 'x', ls(10).charAt(0))
    cube(9)  = Array(ls(4).charAt(8), 'x', ls(10).charAt(2))
    cube(10) = Array(ls(4).charAt(2), 'x', ls(4).charAt(3))
    cube(11) = Array(ls(4).charAt(6), 'x', ls(4).charAt(5))

    cube(13) = Array('x',             ls(8).charAt(1), ls(9).charAt(1))
    cube(15) = Array(ls(5).charAt(1), ls(7).charAt(0), 'x')
    cube(16) = Array(ls(5).charAt(7), ls(7).charAt(2), 'x')
    cube(18) = Array('x',             ls(6).charAt(1), ls(5).charAt(4))

    // Return the arranged Cube
    cube
  }

  /**
   * Fills a cube instance to represent the solved state.
   *
   * @return  a solved Rubik's Cube
   */
  def setSolvedCube(): Cube = {
    val solvedCube = Array.ofDim[Cubie](20)

    // Corner cubies
    //                      x    y    z
    solvedCube(0)  = Array('G', 'R', 'W')
    solvedCube(2)  = Array('B', 'R', 'W')
    solvedCube(5)  = Array('G', 'R', 'Y')
    solvedCube(7)  = Array('B', 'R', 'Y')

    solvedCube(12) = Array('G', 'O', 'W')
    solvedCube(14) = Array('B', 'O', 'W')
    solvedCube(17) = Array('G', 'O', 'Y')
    solvedCube(19) = Array('B', 'O', 'Y')

    // Side cubies
    solvedCube(1)  = Array('x', 'R', 'W')
    solvedCube(3)  = Array('G', 'R', 'x')
    solvedCube(4)  = Array('B', 'R', 'x')
    solvedCube(6)  = Array('x', 'R', 'Y')

    solvedCube(8)  = Array('G', 'x', 'W')
    solvedCube(9)  = Array('B', 'x', 'W')
    solvedCube(10) = Array('G', 'x', 'Y')
    solvedCube(11) = Array('B', 'x', 'Y')

    solvedCube(13) = Array('x', 'O', 'W')
    solvedCube(15) = Array('G', 'O', 'x')
    solvedCube(16) = Array('B', 'O', 'x')
    solvedCube(18) = Array('x', 'O', 'Y')

    // Return the finished Cube
    solvedCube
  }

  /**
   * An array of all the corner cubies in the given Cube.
   *
   * @param c  a cube
   * @return  an array of the corner cubies
   */
  def getCubeCornerList(c: Cube): Array[Cubie] = Array(c(0), c(2), c(5), c(7), c(12), c(14), c(17), c(19))

  /**
   * An array of all the side cubies in the given Cube.
   *
   * @param c  a cube
   * @return   an array of the side cubies
   */
  def getCubeSideList(c: Cube): Array[Cubie] = Array(c(1), c(3), c(4), c(6), c(8), c(9), c(10), c(11), c(13), c(15), c(16), c(18))

  /**
   * An array of the first 6 side cubies in the given cube.
   *
   * @param c  a cube
   * @return   an array of the first 6 side cubies
   */
  def getFirstHalfOfCubeSides(c: Cube): Array[Cubie] = Array(c(1), c(3), c(4), c(6), c(8), c(9))

  /**
   * An array of the last 6 side cubies in the given cube.
   *
   * @param c  a cube
   * @return   an array of the first 6 side cubies
   */
  def getSecondHalfOfCubeSides(c: Cube): Array[Cubie] = Array(c(10), c(11), c(13), c(15), c(16), c(18))

  /**
   * Gets the orientation of a corner Cubie.
   *
   * Inspired by the corner parity test in Rubiks-Cube-Validator, but instead of totaling the parities,
   * we just check one cubie for whether its parity is 0, 1, or 2 (so, whether its position value is
   * 0, 1, or 2).
   *
   * @param corner  the input Cubie
   * @return  the orientation value for the Cubie
   */
  def getCornerParity(corner: Cubie, cList: Array[Cubie]): Int =  {

    // This represents one clockwise turn, but is different depending on which side of the Cube
    // our Cubie is (from a fixed perspective of yellow in front, red on top)
    def shiftOnLeft(c: Cubie): Cubie  = Array(c(2), c(0), c(1))
    def shiftOnRight(c: Cubie): Cubie = Array(c(1), c(2), c(0))

    def calcParity(corner: Cubie, parity: Int, isLeftSide: Boolean, isOnFront: Boolean, isTop: Boolean): Int = {
      if (corner(1) == 'R' || corner(1) == 'O') parity
      else if (isLeftSide  && isOnFront  && isTop) calcParity(shiftOnLeft(corner), parity+1, isLeftSide, isOnFront, isTop)
      else if (isLeftSide  && !isOnFront && isTop) calcParity(shiftOnRight(corner), parity+1, isLeftSide, isOnFront, isTop)
      else if (!isLeftSide && isOnFront  && isTop) calcParity(shiftOnRight(corner), parity+1, isLeftSide, isOnFront, isTop)
      else if (!isLeftSide && !isOnFront && isTop) calcParity(shiftOnLeft(corner), parity+1, isLeftSide, isOnFront, isTop)
      else if (isLeftSide  && isOnFront  && !isTop) calcParity(shiftOnRight(corner), parity+1, isLeftSide, isOnFront, isTop)
      else if (isLeftSide  && !isOnFront && !isTop) calcParity(shiftOnLeft(corner), parity+1, isLeftSide, isOnFront, isTop)
      else if (!isLeftSide && isOnFront  && !isTop) calcParity(shiftOnLeft(corner), parity+1, isLeftSide, isOnFront, isTop)
      else if (!isLeftSide && !isOnFront && !isTop) calcParity(shiftOnRight(corner), parity+1, isLeftSide, isOnFront, isTop)
      else -1
    }

    calcParity(corner, 0, isLeftSideCubie(corner, cList), isOnFrontOfCube(corner, cList), isOnTopOfCube(corner, cList))
  }

  def isLeftSideCubie(inCubie: Cubie, inputCorners: Array[Cubie]) = {
    inputCorners.indexOf(inCubie) == 0 ||
      inputCorners.indexOf(inCubie) == 2 ||
      inputCorners.indexOf(inCubie) == 4 ||
      inputCorners.indexOf(inCubie) == 6
  }

  def isOnFrontOfCube(inCubie: Cubie, inputCorners: Array[Cubie]) = {
    inputCorners.indexOf(inCubie) == 2 ||
      inputCorners.indexOf(inCubie) == 3 ||
      inputCorners.indexOf(inCubie) == 6 ||
      inputCorners.indexOf(inCubie) == 7
  }

  def isOnTopOfCube(inCubie: Cubie, inputCorners: Array[Cubie]) = {
    inputCorners.indexOf(inCubie) >= 0 &&
      inputCorners.indexOf(inCubie) <= 3
  }

  /**
   * Gets the orientation of a side Cubie.
   * Inspired by the side parity test in Rubiks-Cube-Validator, but instead of totaling the parities,
   * we just check one cubie for whether its parity is 0 or 1 (so, whether its position value is 0 or 1).
   *
   * @param c  the input Cubie
   * @return  the orientation value for the Cubie
   */
  def getSideParity(c: Cubie): Int = {
    // Only for cubies in the top or bottom layers
    if (c(1) != 'x') {

      // Rule 1: if the y-axis is R or O, we're good. All we need to do to get the Cubie in its right position
      //         (so also taking into account the other color on the Cubie) is spin the top or bottom face. Since
      //         it only takes spins on L, R, U, or D (here, only on U or D), then the parity is 0.
      if (c(1) == 'R' || c(1) == 'O') 0

      // Rule 2: if the the x- or z-axis is R or O, we're bad. You can't get it home without using an F or B move.
      else if (c(0) == 'R' || c(0) == 'O') 1
      else if (c(2) == 'R' || c(2) == 'O') 1

      // Rule 3a: it is bad if it has Y or W facing the sides. They require at least one F or B to fix.
      // Rule 3b: it is good if it has B or G facing the sides.
      else if (c(0) == 'Y' || c(0) == 'W' || c(2) == 'Y' || c(2) == 'W') 1
      else if (c(0) == 'B' || c(0) == 'G' || c(2) == 'B' || c(2) == 'G') 0

      else 0

    // Only for cubies in the middle layer
    } else {
      // Rule 4a: if it has R or O facing front or back, then we're good.
      // Rule 4b: if it has R or O facing left or right, then we're bad.
      if      (c(2) == 'R' || c(2) == 'O') 0
      else if (c(0) == 'R' || c(0) == 'O') 1

      // Rule 5a: if it has W or Y facing front or back, then we're good.
      // Rule 5b: if it has W or Y facing left or right, then we're bad.
      else if (c(2) == 'W' || c(2) == 'Y') 0
      else if (c(0) == 'W' || c(0) == 'Y') 1

      else 0
    }
  }

  /*
   *
   * Functions for making legal moves on a cube.
   *
   */

  /**
   * Makes an 'up' turn on the cube.
   *
   * @param cube  The Rubik's Cube
   * @param turn  The number of clockwise quarter-turns we are making to the face (1, 2, or 3)
   * @return      The cube with the turn applied
   */
  def turn_U(cube: Cube, turn: Int): Cube = {

    val tempcube = cube.clone()

    def orient(cubie: Cubie): Cubie = Array(cubie(2), cubie(1), cubie(0))

    for (i <- 0 until turn) {
      val tempCorner = orient(tempcube(0))
      tempcube(0) = orient(tempcube(5))
      tempcube(5) = orient(tempcube(7))
      tempcube(7) = orient(tempcube(2))
      tempcube(2) = tempCorner

      val tempSide = orient(tempcube(1))
      tempcube(1) = orient(tempcube(3))
      tempcube(3) = orient(tempcube(6))
      tempcube(6) = orient(tempcube(4))
      tempcube(4) = tempSide
    }
    tempcube
  }

  /**
   * Makes a 'down' turn on the cube.
   *
   * @param cube  The Rubik's Cube
   * @param turn  The number of clockwise quarter-turns we are making to the face (1, 2, or 3)
   * @return      The cube with the turn applied
   */
  def turn_D(cube: Cube, turn: Int): Cube = {

    val tempcube = cube.clone()

    def orient(cubie: Cubie): Cubie = Array(cubie(2), cubie(1), cubie(0))

    for (i <- 0 until turn) {
      val tempCorner = orient(tempcube(17))
      tempcube(17) = orient(tempcube(12))
      tempcube(12) = orient(tempcube(14))
      tempcube(14) = orient(tempcube(19))
      tempcube(19) = tempCorner

      val tempSide = orient(tempcube(18))
      tempcube(18) = orient(tempcube(15))
      tempcube(15) = orient(tempcube(13))
      tempcube(13) = orient(tempcube(16))
      tempcube(16) = tempSide
    }
    tempcube
  }

  /**
   * Makes a 'left' turn on the cube.
   *
   * @param cube  The Rubik's Cube
   * @param turn  The number of clockwise quarter-turns we are making to the face (1, 2, or 3)
   * @return      The cube with the turn applied
   */
  def turn_L(cube: Cube, turn: Int): Cube = {

    val tempcube = cube.clone()

    def orient(cubie: Cubie): Cubie = Array(cubie(0), cubie(2), cubie(1))

    for (i <- 0 until turn) {
      val tempCorner = orient(tempcube(0))
      tempcube(0) = orient(tempcube(12))
      tempcube(12) = orient(tempcube(17))
      tempcube(17) = orient(tempcube(5))
      tempcube(5) = tempCorner

      val tempSide = orient(tempcube(3))
      tempcube(3) = orient(tempcube(8))
      tempcube(8) = orient(tempcube(15))
      tempcube(15) = orient(tempcube(10))
      tempcube(10) = tempSide
    }
    tempcube
  }

  /**
   * Makes a 'right' turn on the cube.
   *
   * @param cube  The Rubik's Cube
   * @param turn  The number of clockwise quarter-turns we are making to the face (1, 2, or 3)
   * @return      The cube with the turn applied
   */
  def turn_R(cube: Cube, turn: Int): Cube = {

    val tempcube = cube.clone()

    def orient(cubie: Cubie): Cubie = Array(cubie(0), cubie(2), cubie(1))

    for (i <- 0 until turn) {
      val tempCorner = orient(tempcube(7))
      tempcube(7) = orient(tempcube(19))
      tempcube(19) = orient(tempcube(14))
      tempcube(14) = orient(tempcube(2))
      tempcube(2) = tempCorner

      val tempSide = orient(tempcube(4))
      tempcube(4) = orient(tempcube(11))
      tempcube(11) = orient(tempcube(16))
      tempcube(16) = orient(tempcube(9))
      tempcube(9) = tempSide
    }
    tempcube
  }

  /**
   * Makes a 'front' turn on the cube.
   *
   * @param cube  The Rubik's Cube
   * @param turn  The number of clockwise quarter-turns we are making to the face (1, 2, or 3)
   * @return      The cube with the turn applied
   */
  def turn_F(cube: Cube, turn: Int): Cube = {

    val tempcube = cube.clone()

    def orient(cubie: Cubie): Cubie = Array(cubie(1), cubie(0), cubie(2))

    for (i <- 0 until turn) {
      val tempCorner = orient(tempcube(5))
      tempcube(5) = orient(tempcube(17))
      tempcube(17) = orient(tempcube(19))
      tempcube(19) = orient(tempcube(7))
      tempcube(7) = tempCorner

      val tempSide = orient(tempcube(6))
      tempcube(6) = orient(tempcube(10))
      tempcube(10) = orient(tempcube(18))
      tempcube(18) = orient(tempcube(11))
      tempcube(11) = tempSide
    }
    tempcube
  }

  /**
   * Makes a 'back' turn on the cube.
   *
   * @param cube  The Rubik's Cube
   * @param turn  The number of clockwise quarter-turns we are making to the face (1, 2, or 3)
   * @return      The cube with the turn applied
   */
  def turn_B(cube: Cube, turn: Int): Cube = {

    val tempcube = cube.clone()

    def orient(cubie: Cubie): Cubie = Array(cubie(1), cubie(0), cubie(2))

    for (i <- 0 until turn) {
      val tempCorner = orient(tempcube(2))
      tempcube(2) = orient(tempcube(14))
      tempcube(14) = orient(tempcube(12))
      tempcube(12) = orient(tempcube(0))
      tempcube(0) = tempCorner

      val tempSide = orient(tempcube(1))
      tempcube(1) = orient(tempcube(9))
      tempcube(9) = orient(tempcube(13))
      tempcube(13) = orient(tempcube(8))
      tempcube(8) = tempSide
    }
    tempcube
  }

  /*
   *
   * Utility functions, general purpose.
   *
   */

  def factorial(n: Int): Int = n match {
    case 0 => 1
    case _ => n * factorial(n-1)
  }
}