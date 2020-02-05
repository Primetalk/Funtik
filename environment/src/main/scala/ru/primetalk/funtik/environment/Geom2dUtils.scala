package ru.primetalk.funtik.environment

object Geom2dUtils {
  type Position = (Int, Int)
  type Vector2d = (Int, Int)
  type Direction = Vector2d

  case class DirVector(direction: Direction, length: Int) {
    def toVector2d: Vector2d = direction * length
    def isHorizontal: Boolean = direction._2 == 0
    def isVertical: Boolean = direction._1 == 0
    /** converts line segment to points.
      * NB! Does not include start position! This is convenient when drawing many segments*/
    def drawPrependFromStart(start: Position, positions: List[Position] = Nil): List[Position] = {
      @scala.annotation.tailrec
      def loop(pos: Position, dir: Direction, len: Int, positions: List[Position]): List[Position] =
        if(len == 0)
          positions
        else {
          val newPos = pos + dir
          loop(newPos, dir, len - 1, newPos :: positions)
        }
      loop(start, direction, length, positions)
    }
  }

  case class LineSegment(start: Position, dirVector: DirVector) {
    def end: Position = start + dirVector.toVector2d
    /** converts line segment to points.
      * NB! Does not include start position! This is convenient when drawing many segments*/
    def drawPrepend(positions: List[Position] = Nil): List[Position] =
      dirVector.drawPrependFromStart(start, positions)
  }

  def rectangleByDiagonal(topLeft: Position, bottomRight: Position): Rectangle = {
    Rectangle(topLeft, bottomRight - topLeft + (1, 1))
  }

  /** Origin is in top left corner. */
  case class Rectangle(topLeft: Position, size: Vector2d) {

    def area: Long =
      size._1 * size._2

    def coordinatePoints: Seq[Position] = Seq(topLeft, bottomRight)

    def bottomRight: Position = topLeft + size - (1, 1)
  }
  /** It's a matrix:
    *  /     \
    *  | a b |
    *  | c d |
    *  \     /
    */
  case class Matrix2d(a: Int, b: Int, c: Int, d: Int)

  // Here is the group of rotations by 90 degrees:
  val rotateRight = Matrix2d( 0, 1,-1, 0)
  val rotateLeft  = Matrix2d( 0,-1, 1, 0)
  val rotateId    = Matrix2d( 1, 0, 0, 1)
  val rotate180   = Matrix2d(-1, 0, 0,-1)

  val rotations = List(rotateId, rotateRight, rotate180, rotateLeft)

  implicit class Matrix2dOps(rot: Matrix2d) {
    def apply(p: Vector2d): Vector2d = (
      rot.a*p._1 + rot.b*p._2,
      rot.c*p._1 + rot.d*p._2
    )
    def apply(other: Matrix2d): Matrix2d = Matrix2d(
      a = rot.a*other.a+rot.b*other.c, b = rot.a*other.b+rot.b*other.d,
      c = rot.c*other.a+rot.d*other.c, d = rot.c*other.b+rot.d*other.d
    )
    def *(other: Matrix2d): Matrix2d = Matrix2d(
      a = rot.a*other.a+rot.b*other.c, b = rot.a*other.b+rot.b*other.d,
      c = rot.c*other.a+rot.d*other.c, d = rot.c*other.b+rot.d*other.d
    )
  }
  /** Finds bounding rectangle for a collection of points. */
  def boundingRect(positions: Seq[Position]): Rectangle = {
    val xs = positions.map(_._1)
    val ys = positions.map(_._2)
    rectangleByDiagonal(
      topLeft = (xs.min, ys.min),
      bottomRight = (xs.max, ys.max)
    )
  }

  val origin: Position = (0, 0)

  val Up: Direction = (0, +1)
  val Down: Direction = (0, -1)
  val Left: Direction = (-1, 0)
  val Right: Direction = (1, 0)

  lazy val mainDirections: Seq[Direction] = Seq(Up, Left, Down, Right)
  lazy val mainDirectionsInReadingOrder: Seq[Direction] = Seq(Up, Left, Right, Down)
  lazy val directions8: Seq[Direction] = mainDirections ++ Seq(Up + Right, Up + Left, Down + Left, Down + Right)

  def mul(k: Int): Direction => Vector2d = {
    case (x, y) => (x * k, y * k)
  }

  implicit class PosOps(p: Position) {
    def +(vector: Vector2d): Position =
      (p._1 + vector._1, p._2 + vector._2)
    def -(vector: Vector2d): Position =
      (p._1 - vector._1, p._2 - vector._2)
  }

  implicit class VecOps(v: Vector2d) {

    def *(k: Int): Vector2d = v match {
      case (x, y) => (x * k, y * k)
    }

    def transpose: Vector2d =
      (v._2, v._1)

    /** Rotates as a multiplication of complex numbers. */
    def rotate(o: Vector2d): Vector2d =
      (v._1 * o._1 - v._2 * o._2, v._1 * o._2 + v._2 * o._1)

    def manhattanSize: Int = math.abs(v._1) + math.abs(v._2)

    def r: Double =
      math.sqrt(v._1*v._1 + v._2*v._2)

    /** Theta is an angle from X axis towards the given vector.
      * NB! The display has Y axis oriented down. So, in order to get normal
      * theta we inverse Y.*/
    def theta: Double = {
      math.atan2(-v._2, v._1)
    }

  }

  def manhattanDistance(p1: Position, p2: Position): Int = {
    math.abs(p1._1 - p2._1) + math.abs(p1._2 - p2._2)
  }

  val readingOrdering: Ordering[Position] =
    (a: (Int, Int), b: (Int, Int)) => {
      val cmp1 = a._2 - b._2
      if(cmp1 != 0)
        cmp1
      else
        a._1 - b._1
    }
  type ManhattanPosition = (Int, Int)

  /**
    * Manhattan affine transform is applied - rotate +45° and scale by sqrt(2)/2.
    */
  def manhattanTransform(p: Position): ManhattanPosition = {
    (p._2 + p._1, p._2 - p._1)
  }
  /** It's a rectangle that is constructed by diagonals.
    * For this an affine transform is applied - rotate +45° and scale by sqrt(2)/2.
    * It's also enough to describe it with just two points.
    */
  case class ManhattanEllipse(p1: Position, p2: Position) {
    def size: Vector2d =
      manhattanTransform(p2 - p1)
  }

  def manhattanCircle(p: Position, r: Int): ManhattanEllipse = {
    ManhattanEllipse(p - (r,0), p + (r,0))
  }

  def charToDirection(c: Char): Direction = c match {
    case 'U' => Up
    case 'D' => Down
    case 'L' => Left
    case 'R' => Right
  }


}
