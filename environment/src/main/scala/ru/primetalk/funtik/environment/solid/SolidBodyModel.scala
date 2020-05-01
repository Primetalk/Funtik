package ru.primetalk.funtik.environment.solid

import ru.primetalk.funtik.environment.geom2d.{CollisionShape, Vector2d, lines2d}
import squants.space.{Length, LengthUnit, Meters}
import squants.time.{Seconds, Time, TimeUnit}
import spire.syntax.all._
import ru.primetalk.funtik.environment.geom2d.Vector._
import ru.primetalk.funtik.environment.geom2d.lines2d.ParametricLine
import Ordering.Double.TotalOrdering
/**
 * This object contains a simple kinematic model of a solid body that has mass=0.
 */
object SolidBodyModel {
  type Axis = Double
  type Vector = Vector2d[Axis]
//  type Time = Double
  /** A system of units that specify measurement units that are used as default in
   * calculations. The values are stored dimensionless. */
  case class SystemOfUnits(length: Length = Meters(1), time: Time = Seconds(1))
  implicit val su: SystemOfUnits = SystemOfUnits()
  case class MaterialParticleState(position: Vector, speed: Vector, t: Double)(implicit su: SystemOfUnits) {
    def trajectory: ParametricLine[Double] = ParametricLine(position, speed)
    /** Calculate new position in time = t1. */
    def integrate(t1: Double): MaterialParticleState = {
      MaterialParticleState(
        position = position + (speed :* (t1 - t)),
        speed = speed,
        t = t1
      )
    }

    def detectNearestCollision(shape: CollisionShape[Axis]): Option[Double] = shape match {
      case CollisionShape.Line(p1, p2) =>
        val l2 = lines2d.TwoPointsLine(p1, p2).toParametricLineDouble
        lines2d.intersection(trajectory, l2).flatMap{ case (t1, _) =>
          Option.when(t1 > t)(t1)
        }
      case CollisionShape.LineSegment(p1, p2) =>
        val l2 = lines2d.TwoPointsLine(p1, p2).toParametricLineDouble
        lines2d.intersection(trajectory, l2).flatMap{ case (t1, t2) =>
          Option.when(t1 > t &&
            t2 >=0 && t2<=1 // we intersect between line segment ends
          )(t1)
        }
      case CollisionShape.OrthogonalSquare(center, side) =>
        val x1 = center.x - side / 2
        val x2 = center.x + side / 2
        val y1 = center.y - side / 2
        val y2 = center.y + side / 2
        val lineSegments = List(
          CollisionShape.LineSegment(Vector2d(x1,y1), Vector2d(x2, y1)),
          CollisionShape.LineSegment(Vector2d(x1,y1), Vector2d(x1, y2)),
          CollisionShape.LineSegment(Vector2d(x2,y1), Vector2d(x2, y2)),
          CollisionShape.LineSegment(Vector2d(x1,y2), Vector2d(x2, y2))
        )
        lineSegments.flatMap(detectNearestCollision).minOption
      case CollisionShape.Circle(center, radius) => // See  Weisstein, Eric W. "Circle-Line Intersection." From MathWorld--A Wolfram Web Resource. https://mathworld.wolfram.com/Circle-LineIntersection.html
        val l1 = trajectory.toTwoPointsLine
        val l = lines2d.TwoPointsLine(l1.p1 - center, l1.p2 - center)
        val dx = l.p2.x - l.p1.x
        val dy = l.p2.y - l.p1.y
        val dr = (l.p2 - l.p1).length
        val D = l.p1.x * l.p2.y - l.p2.x * l.p1.y
        val discriminant = radius * radius * dr * dr - D * D
        Option.when(discriminant >= 0) {
          val x1 = D * dy + math.signum(dy) * dx * math.sqrt(discriminant)
          val y1 = - D * dx + math.abs(dy) * math.sqrt(discriminant)
          val x2 = D * dy - math.signum(dy) * dx * math.sqrt(discriminant)
          val y2 = - D * dx - math.abs(dy) * math.sqrt(discriminant)
          val p1 = Vector2d[Double](x1, y1) + center
          val p2 = Vector2d[Double](x2, y2) + center
          val points = List(p1, p2).
            map(p => trajectory.pointToParameter(p)).
            filter(_ >= t)
          points.minOption
        }.flatten
    }
    def setSpeed(t1: Double, speed: Vector): MaterialParticleState =
      integrate(t1).copy(speed = speed)
  }
  /** State of a solid body includes angle theta and the speed at which this angle is changing. */
  case class SolidBodyState(materialParticle: MaterialParticleState, theta: Double, omega: Double) {
    /** Calculate new position in time = t1. */
    def integrate(t1: Double): SolidBodyState =
      SolidBodyState(
        materialParticle = materialParticle.integrate(t1),
        theta = theta + omega * (t1 - materialParticle.t),
        omega = omega
      )

    def setOmega(t1: Double, omega: Double): SolidBodyState =
      integrate(t1).copy(omega = omega)

  }
}
