package ru.primetalk.funtik.environment.solid

import ru.primetalk.funtik.environment.geom2d.{CollisionShape, Trajectory, TrajectoryCoordinates, Vector2d, Vector2dPolar, lines2d}
import squants.space.{Length, Meters}
import squants.time.{Seconds, Time}
import spire.syntax.all._
import ru.primetalk.funtik.environment.geom2d.Vector._

import Ordering.Double.TotalOrdering

case class SystemOfUnits(length: Length = Meters(1), time: Time = Seconds(1))

sealed trait MaterialParticleStateCommand

object MaterialParticleStateCommand {
  case class IntegrateUpToTime(t: Double) extends MaterialParticleStateCommand
  case class SetSpeedAndAcceleration(
    speed: Vector2d[Double],
    orthogonalAcceleration: Double,
  ) extends MaterialParticleStateCommand
}
/**
 * @param orthogonalAcceleration is orthogonal to the speed vector. If it's positive, then robot turns right,
 *                               if negative - left, if it's zero, robot is moving straight.
 *                               For integration we use circle
 */
case class MaterialParticleState(position: Vector2d[Double], speed: Vector2d[Double],
                                 orthogonalAcceleration: Double, t: Double)(implicit su: SystemOfUnits) {
  def trajectory: Trajectory =
    if(orthogonalAcceleration == 0.0)
      Trajectory.Linear(position, speed, t)
    else {
      val polarSpeed = speed.toPolar
      val v = polarSpeed.r
      val a = math.abs(orthogonalAcceleration)
      val r = v * v / a
      val phase = polarSpeed.theta - math.Pi / 2 + (if(orthogonalAcceleration >= 0) 0 else math.Pi)

      val relativePosition = Vector2dPolar(r, phase).toVector2d
      val center = position - relativePosition
      val omega = v / r * Trajectory.sgn(orthogonalAcceleration)

      Trajectory.Circular(
        center = center,
        radius = r,
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

  def detectNearestCollision(shape: CollisionShape[Double]): Option[TrajectoryCoordinates] =
    Trajectory.detectNearestCollision(trajectory, shape)

  def setSpeed(t1: Double, speed: Vector2d[Double]): MaterialParticleState =
    integrate(t1).copy(speed = speed)

  def handleCommand(materialParticleStateCommand: MaterialParticleStateCommand): MaterialParticleState = materialParticleStateCommand match {
    case MaterialParticleStateCommand.IntegrateUpToTime(t) => integrate(t)
    case MaterialParticleStateCommand.SetSpeedAndAcceleration(speed, orthogonalAcceleration) =>
      copy(speed = speed, orthogonalAcceleration = orthogonalAcceleration)
  }

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

  def handleCommand(materialParticleStateCommand: MaterialParticleStateCommand): SolidBodyState = {
    val materialParticleState2 = materialParticle.handleCommand(materialParticleStateCommand)
    val polar = materialParticleState2.speed.toPolar
    val theta1 = if(polar.r == 0.0) theta else polar.theta

    SolidBodyState(
      materialParticle = materialParticleState2,
      theta = theta1
    )
  }

}

case class SolidBodyRotationParameters(tangentialVelocity: Double, orthogonalAcceleration: Double)

/**
 * This object contains a simple kinematic model of a solid body that has mass=0.
 */
object SolidBodyModel {
  type Axis = Double
  type Vector = Vector2d[Axis]
//  type Time = Double
  /** A system of units that specify measurement units that are used as default in
   * calculations. The values are stored dimensionless. */
  implicit val su: SystemOfUnits = SystemOfUnits()

}
