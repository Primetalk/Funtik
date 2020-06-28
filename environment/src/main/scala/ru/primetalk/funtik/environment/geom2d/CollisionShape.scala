package ru.primetalk.funtik.environment.geom2d

import ru.primetalk.funtik.environment.Display

sealed trait CollisionShape[Axis]

object CollisionShape {

  case class Line[Axis](p1: Vector2d[Axis], p2: Vector2d[Axis]) extends CollisionShape[Axis]

  case class LineSegment[Axis](p1: Vector2d[Axis], p2: Vector2d[Axis]) extends CollisionShape[Axis] {
    def toLine: Line[Axis] = Line(p1, p2)
  }

  case class Circle[Axis](center: Vector2d[Axis], radius: Axis) extends CollisionShape[Axis]

  case class Polygon[Axis](points: List[Vector2d[Axis]]) extends CollisionShape[Axis] {
    def toLineSegments: Seq[LineSegment[Axis]] =
      if(points.length < 2)
        Seq()
      else
        (points :+ points.head).
          sliding(2,1).
          map{
            case Seq(a, b) =>
              LineSegment(a, b)
          }.
          toSeq
  }

  def polygon[Axis](points: Vector2d[Axis]*): Seq[LineSegment[Axis]] =
    Polygon(points.toList).toLineSegments

  def rectangle[Axis](p1: Vector2d[Axis], p2: Vector2d[Axis]): Seq[LineSegment[Axis]] = {
    val Vector2d(x1, y1) = p1
    val Vector2d(x2, y2) = p2
    polygon(
      Vector2d(x1, y1),
      Vector2d(x2, y1),
      Vector2d(x2, y2),
      Vector2d(x1, y2),
    )
  }

  /** OrthogonalSquare has 4 sides parallel to axes.  */
  def orthogonalSquare(center: Vector2d[Double], side: Double): Polygon[Double] = {
    val x1 = center.x - side / 2
    val x2 = center.x + side / 2
    val y1 = center.y - side / 2
    val y2 = center.y + side / 2
    Polygon(List(Vector2d(x1,y1), Vector2d(x2, y1), Vector2d(x2, y2), Vector2d(x1,y2)))
  }

  /** Two points - top left and botton right */
  type BoundingRect[Axis] = (Vector2d[Axis], Vector2d[Axis])

  def pointToBoundingRect[Axis](p: Vector2d[Axis])(implicit n: Numeric[Axis]): BoundingRect[Axis] = {
    import n.NumericOps
    (p, Vector2d(n.minus(p.x, n.one), n.minus(p.y, n.one)))
  }

  def combineBoundingRects[Axis](r1: BoundingRect[Axis], r2: BoundingRect[Axis])(implicit n: Numeric[Axis]): BoundingRect[Axis] =
    (
      Vector2d(n.min(r1._1.x, r2._1.x), n.min(r1._1.y, r2._1.y)),
      Vector2d(n.max(r1._2.x, r2._2.x), n.max(r1._2.y, r2._2.y)),
    )

  def listOfPointsToBoundingRect[Axis](ls: List[Vector2d[Axis]])(implicit n: Numeric[Axis]): BoundingRect[Axis] =
    ls.map(pointToBoundingRect(_)).reduceLeft(combineBoundingRects(_, _))

  def boundingRect[Axis](collisionShape: CollisionShape[Axis])(implicit n: Numeric[Axis]): BoundingRect[Axis] = collisionShape match {
    case Line(p1, p2) =>
      listOfPointsToBoundingRect(List(p1, p2))
    case LineSegment(p1, p2) =>
      listOfPointsToBoundingRect(List(p1, p2))
    case Circle(center, radius) =>
      import n.NumericOps
      (
        Vector2d(new NumericOps(center.x) - radius, new NumericOps(center.y) - radius),
        Vector2d(new NumericOps(center.x) + radius, new NumericOps(center.y) + radius),
      )
    case Polygon(points) =>
      listOfPointsToBoundingRect(points)
  }

  def toPoints[Axis](collisionShape: CollisionShape[Axis])(implicit n: Numeric[Axis]): List[Vector2d[Int]] = collisionShape match {
    case Line(p1, p2) =>
      toPoints(LineSegment(p1, p2))
    case LineSegment(p1, p2) =>
      Geom2dUtils.bresenhamLine(n.toInt(p1.x), n.toInt(p1.y), n.toInt(p2.x), n.toInt(p2.y))
    case Circle(center, radius) =>
      ???
    case Polygon(points) =>
      Polygon(points).toLineSegments.flatMap(toPoints(_)).toList
  }
}
