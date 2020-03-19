package ru.primetalk.funtik.environment.geom2d
import Axis2d._


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

  /**
   * An algorithm to draw pixels for a mathematical line.
   * https://ru.wikipedia.org/wiki/Алгоритм_Брезенхэма
   * {{{
   *     function line(int x0, int x1, int y0, int y1)
   *      int deltax := abs(x1 - x0)
   *      int deltay := abs(y1 - y0)
   *      int error := 0
   *      int deltaerr := (deltay + 1)
   *      int y := y0
   *      int diry := y1 - y0
   *      if diry > 0
   *          diry = 1
   *      if diry < 0
   *          diry = -1
   *      for x from x0 to x1
   *          plot(x,y)
   *          error := error + deltaerr
   *          if error >= (deltax + 1)
   *              y := y + diry
   *              error := error - (deltax + 1)
   * }}}
   */
  def bresenhamLine(x0: Double, y0: Double, x1: Double, y1: Double): List[Position] = {
    val deltaX = math.abs(x1 - x0)
    val deltaY = math.abs(y1 - y0)
    val deltaErr = deltaY + 1
    val dirX = if(x1 > x0) 1 else -1
    val dirY = if(y1 > y0) 1 else -1
    @scala.annotation.tailrec
    def loop(x: Int, y: Int, error: Double, reversed: List[Position]): List[Position] = {
      if(if(dirX > 0) x > x1.round else x < x1.round)
        reversed.reverse
      else {
        val res1 = vector2d(x, y) :: reversed
        val error1 = error + deltaErr

        if(error1 >= deltaX + 1)
          loop(x + dirX, y + dirY, error1 - (deltaX + 1), res1)
        else
          loop(x + dirX, y, error1, res1)
      }
    }
    if(deltaX >= deltaY)
      loop(x0.round.toInt, y0.round.toInt, y0 - y0.round, Nil)
    else
      bresenhamLine(y0, x0, y1, x1).map(_.transpose)
  }

  def line(from: Position, to: Position): List[Position] = {
    bresenhamLine(from.x + 0.5, from.y + 0.5, to.x + 0.5, to.y + 0.5)
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

    def width: Int = size.x

    def height: Int = size.y

    def coordinatePoints: Seq[Position] = Seq(topLeft, bottomRight)

    def bottomRight: Position = topLeft + size - vector2d(1, 1)

    def topRight: Position = topLeft + vector2d(width - 1, 0)

    def bottomLeft: Position = topLeft + vector2d(0, height - 1)

    def edges: List[Position] = List(
      line(topLeft, topRight),
      line(topRight, bottomRight),
      line(bottomRight, bottomLeft),
      line(bottomLeft, topLeft)
    ).flatten

    def sideSize(axis: Axis2d): Int = axis match {
      case Horizontal => width
      case Vertical => height
    }

    def getLongestAxis: Option[Axis2d] = {
      if (height > width) {
        Some(Vertical)
      } else if (width > height) {
        Some(Horizontal)
      } else {
        None
      }
    }

    def split(axis: Axis2d, firstAxisSize: Int): (Rectangle, Rectangle) = {

      val firstSize = axis match {
        case Horizontal => vector2d(width, firstAxisSize)
        case Vertical => vector2d(firstAxisSize, height)
      }
      val first = Rectangle(topLeft, firstSize)

      val secondStart = axis match {
        case Horizontal =>
          vector2d(topLeft.x, topLeft.y + firstAxisSize - 1)
        case Vertical =>
          vector2d(topLeft.x + firstAxisSize - 1, topLeft.y)
      }

      val secondSize = axis match {
        case Horizontal =>
          vector2d(width, height - firstAxisSize + 1)
        case Vertical =>
          vector2d(width - firstAxisSize + 1, height)
      }
      val second = Rectangle(secondStart, secondSize)

      (first, second)
    }
  }

  // Here is the group of rotations by 90 degrees:
  val rotateRight: Matrix2d = Matrix2d( 0, 1,-1, 0)
  val rotateLeft: Matrix2d  = Matrix2d( 0,-1, 1, 0)
  val rotateId: Matrix2d    = Matrix2d( 1, 0, 0, 1)
  val rotate180: Matrix2d   = Matrix2d(-1, 0, 0,-1)

  val rotations: List[Matrix2d] = List(rotateId, rotateRight, rotate180, rotateLeft)

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
  /**
   * This method converts a one-char representation of direction to
   * ordinary Direction. It could be used for short representation of a path:
   * {{{
   *   ULLLUULDDD
   * }}}
   *
   * or
   * {{{
   *   U1L3U2L1D3
   * }}}
   */
  def charToDirection(c: Char): Direction = c match {
    case 'U' => Up
    case 'D' => Down
    case 'L' => Left
    case 'R' => Right
    case _ => throw new IllegalArgumentException(s"Unsupported short direction char '$c'")
  }


}

object Geom2dUtils extends  Geom2dUtils[(Int, Int)] {
  implicit lazy val vector2d: Vector2d[(Int, Int)] = Vector2dIntPair
}
