package ru.primetalk.funtik.environment.solid

import ru.primetalk.funtik.environment.geom2d.{CollisionShape, Trajectory, Vector2d, Vector2dPolar, lines2d}
import squants.space.{Length, Meters}
import squants.time.{Seconds, Time}
import spire.syntax.all._
import ru.primetalk.funtik.environment.geom2d.Vector._

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

  /**
   * @param orthogonalAcceleration is orthogonal to the speed vector. If it's positive, then robot turns right,
   *                               if negative - left, if it's zero, robot is moving straight.
   *                               For integration we use circle
   */
  case class MaterialParticleState(position: Vector, speed: Vector, orthogonalAcceleration: Double, t: Double)(implicit su: SystemOfUnits) {
    def trajectory: Trajectory =
      if(orthogonalAcceleration == 0.0)
        Trajectory.Linear(position, speed, t)
      else {
        val polarSpeed = speed.toPolar
        val v = polarSpeed.r
        val r = v * v / orthogonalAcceleration
        val phase = polarSpeed.theta - math.Pi / 2 + (if(r > 0) 0 else math.Pi)
        val absR = math.abs(r)
        val center = position + Vector2dPolar(absR, phase).toVector2d
        val omega = v / absR

        Trajectory.Circular(
          center = center,
          radius = absR,
          phase0 = phase,
          t0 = t,
          omega = omega)
      }

    /** Calculate new position in time = t1. */
    def integrate(t1: Double): MaterialParticleState = {
      val (position1, velocity1) = trajectory.integrate(t1)
      this.copy(
        position = position1,
        speed = velocity1,
        t = t1
      )
    }

    def detectNearestCollision(shape: CollisionShape[Axis]): Option[Double] = (trajectory, shape) match {
      case (l1: Trajectory.Linear, CollisionShape.Line(p1, p2)) =>
        val l2 = lines2d.TwoPointsLine(p1, p2).toParametricLineDouble(t)
        Trajectory.intersection(l1, l2).flatMap{ case (t1, _) =>
          Option.when(t1 > t)(t1)
        }.headOption
      case (l1: Trajectory.Linear, CollisionShape.LineSegment(p1, p2)) =>
        val l2 = lines2d.TwoPointsLine(p1, p2).toParametricLineDouble(t)
        Trajectory.intersection(l1, l2).flatMap{ case (t1, t2) =>
          Option.when(t1 > t &&
            t2 >=0 && t2<=1 // we intersect between line segment ends
          )(t1)
        }.headOption
      case (_, CollisionShape.OrthogonalSquare(center, side)) =>
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
      case (l1: Trajectory.Linear, CollisionShape.Circle(center, radius)) => // See  Weisstein, Eric W. "Circle-Line Intersection." From MathWorld--A Wolfram Web Resource. https://mathworld.wolfram.com/Circle-LineIntersection.html
        Trajectory.intersection(Trajectory.Circular(center, radius, 0, 0, 1), l1).
          map(_._2).
          filter(_ >= t).
          minOption
      case (c1: Trajectory.Circular, CollisionShape.Line(p1, p2)) =>
        val l = lines2d.TwoPointsLine(p1 - c1.center, p2 - c1.center)

        Trajectory.intersection(c1, l.toParametricLineDouble(0.0)).
          map(_._1).
          filter(_ >= t).
          minOption
      case (c1: Trajectory.Circular, c2: CollisionShape.Circle[Double]) =>
        val c2t = Trajectory.Circular(c2.center, c2.radius, t, 0.0, 1.0)
        c1.intersect(c2t).map(_._1).filter(_ > t).minOption
    }
    def setSpeed(t1: Double, speed: Vector): MaterialParticleState =
      integrate(t1).copy(speed = speed)
  }
  /** State of a solid body includes angle theta and the speed at which this angle is changing. */
  case class SolidBodyState(materialParticle: MaterialParticleState, theta: Double) {
    /** Calculate new position in time = t1. */
    def integrate(t1: Double): SolidBodyState = {
      val materialParticleState = materialParticle.integrate(t1)
      SolidBodyState(
        materialParticle = materialParticleState,
        theta = materialParticleState.speed.toPolar.theta
      )
    }

//    def setOmega(t1: Double, omega: Double): SolidBodyState =
//      integrate(t1).copy(omega = omega)

  }

  case class RobotGeometry(width: Double, wheelRadius: Double){

    /**
     * Robot has two wheels. We know their radius and the distance between them.
     * Rotational speed of wheels - radians per second - is given.
     * We want to find linear (tangential) speed and rotational speed of the robot.
     * leftLinearSpeed = wheelRadius*leftOmega
     *
     * radius is positive when robot rotates to left (counter clockwise)
     * otherwise it is negative.
     * @return
     */
    def convertTwoWheelsSpeedToSpeedAndOmega(leftOmega: Double, rightOmega: Double): SolidBodyRotationParameters = {
      val leftLinearSpeed = wheelRadius * leftOmega
      val rightLinearSpeed = wheelRadius * rightOmega
      val linearVelocity1 = (leftLinearSpeed + rightLinearSpeed) / 2
      if(leftOmega == rightOmega) {
        SolidBodyRotationParameters(linearVelocity1, 0.0)
      } else {
        // Proportion:
        // rLeft / (rLeft + wheelsDistance) = left / right
        // rLeft (1 - left/right) = left/right * wheelsDistance
        // rLeft = left/(right - left) * wheelsDistance

        // r = leftOmega / (rightOmega - leftOmega) * wheelsDistance

        // rCenter = rLeft + wheelsDistance / 2 = (left + right) / (right - left)* wheelsDistance
        // a = V^2/rCenter = V^2 * (right - left) / (left + right) / wheelsDistance
        val acceleration = linearVelocity1 * linearVelocity1 * (rightOmega - leftOmega) / (leftOmega + rightOmega) / width
        SolidBodyRotationParameters(
          tangentialVelocity = linearVelocity1,
          orthogonalAcceleration = acceleration
        )
      }
    }
  }
  case class SolidBodyRotationParameters(tangentialVelocity: Double, orthogonalAcceleration: Double)
}
