package ru.primetalk.funtik.environment.geom2d

sealed trait CollisionShape[Axis]

object CollisionShape {

  case class Line[Axis](p1: Vector2d[Axis], p2: Vector2d[Axis]) extends CollisionShape[Axis]

  case class LineSegment[Axis](p1: Vector2d[Axis], p2: Vector2d[Axis]) extends CollisionShape[Axis]
  /** OrthogonalSquare has sides parallel to axes.  */
  case class OrthogonalSquare[Axis](center: Vector2d[Axis], side: Axis) extends CollisionShape[Axis]

  case class Circle[Axis](center: Vector2d[Axis], radius: Axis) extends CollisionShape[Axis]
}