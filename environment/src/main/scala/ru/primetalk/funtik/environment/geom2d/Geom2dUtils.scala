package ru.primetalk.funtik.environment.geom2d


trait Geom2dUtils[V] extends Vector2dSyntax[V] {
  type Vector = V
  type Position = Vector
  type Direction = Vector

  implicit val vector2d: Vector2d[Vector]

  case class DirVector(direction: Direction, length: Int) {
    def toVector2d: Vector = direction * length
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
    Rectangle(topLeft, bottomRight - topLeft + vector2d(1, 1))
  }

  /** Origin is in top left corner. */
  case class Rectangle(topLeft: Position, size: Vector) {

    def area: Long =
      size._1 * size._2

    def coordinatePoints: Seq[Position] = Seq(topLeft, bottomRight)

    def bottomRight: Position = topLeft + size - vector2d(1, 1)
  }

  // Here is the group of rotations by 90 degrees:
  val rotateRight: Matrix2d = Matrix2d( 0, 1,-1, 0)
  val rotateLeft: Matrix2d  = Matrix2d( 0,-1, 1, 0)
  val rotateId: Matrix2d    = Matrix2d( 1, 0, 0, 1)
  val rotate180: Matrix2d   = Matrix2d(-1, 0, 0,-1)

  val rotations = List(rotateId, rotateRight, rotate180, rotateLeft)

  implicit class Matrix2dOps(rot: Matrix2d) {
    def apply(p: Vector): Vector = (
      rot.a*p._1 + rot.b*p._2,
      rot.c*p._1 + rot.d*p._2
    ).toVector
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
      topLeft = vector2d(xs.min, ys.min),
      bottomRight = vector2d(xs.max, ys.max)
    )
  }

  val origin: Position = vector2d(0, 0)

  val Up: Direction = vector2d(0, +1)
  val Down: Direction = vector2d(0, -1)
  val Left: Direction = vector2d(-1, 0)
  val Right: Direction = vector2d(1, 0)

  lazy val mainDirections: Seq[Direction] = Seq(Up, Left, Down, Right)
  lazy val mainDirectionsInReadingOrder: Seq[Direction] = Seq(Up, Left, Right, Down)
  lazy val directions8: Seq[Direction] = mainDirections ++ Seq(Up + Right, Up + Left, Down + Left, Down + Right)

  def mul(k: Int)(d: Direction): Vector =
    vector2d(d.x * k, d.y * k)

  def manhattanDistance(p1: Position, p2: Position): Int = {
    math.abs(p1._1 - p2._1) + math.abs(p1._2 - p2._2)
  }

  val readingOrdering: Ordering[Position] =
    (a, b) => {
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
    def size: Vector =
      manhattanTransform(p2 - p1).toVector
  }

  def manhattanCircle(p: Position, r: Int): ManhattanEllipse = {
    ManhattanEllipse(p - vector2d(r,0), p + vector2d(r,0))
  }

  def charToDirection(c: Char): Direction = c match {
    case 'U' => Up
    case 'D' => Down
    case 'L' => Left
    case 'R' => Right
  }


}

object Geom2dUtils extends  Geom2dUtils[(Int, Int)] {
  implicit val vector2d: Vector2d[(Int, Int)] = Vector2dIntPair
}
