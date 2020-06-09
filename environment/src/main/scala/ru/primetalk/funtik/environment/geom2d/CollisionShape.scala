package ru.primetalk.funtik.environment.geom2d

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
}
