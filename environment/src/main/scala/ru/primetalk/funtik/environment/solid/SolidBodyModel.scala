package ru.primetalk.funtik.environment.solid

import ru.primetalk.funtik.environment.geom2d.Vector2d
import spire.algebra.VectorSpace
import squants.space.{Length, Meters}
import squants.QuantityVector
import squants.time.{Seconds, Time}
import spire.syntax.all._
import spire.std.double._

import ru.primetalk.funtik.environment.geom2d.Vector._
/**
 * This object contains a simple kinematic model of a solid body that has mass=0.
 */
object SolidBodyModel {
  type Vector = Vector2d[Double]
//  type Time = Double
  /** A system of units that specify measurement units that are used as default in
   * calculations. The values are stored dimensionless. */
  case class SystemOfUnits(length: Length = Meters(1), time: Time = Seconds(1))
  case class MaterialParticleState(position: Vector, speed: Vector, t: Double)(implicit su: SystemOfUnits) {
    /** Calculate new position in time = t1. */
    def integrate(t1: Double): MaterialParticleState =
      MaterialParticleState(
        position = position + speed :* (t1 - t),
        speed = speed,
        t = t1
      )
    def setSpeed(t1: Double, speed: Vector): MaterialParticleState =
      integrate(t1).copy(speed = speed)
  }
  /** State of a solid body includes angle theta and the speed at which this angle is changing. */
  case class SolidBodyState(materialParticle: MaterialParticleState, theta: Double, omega: Double)(implicit su: SystemOfUnits) {
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
