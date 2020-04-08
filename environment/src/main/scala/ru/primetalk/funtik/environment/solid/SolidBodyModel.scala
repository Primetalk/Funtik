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
  /** A system of units that specify measurement units that are used as default in
   * calculations. The values are stored dimensionless. */
  case class SystemOfUnits(length: Length = Meters(1), time: Time = Seconds(1))
  case class MaterialPointState(r: Vector, speed: Vector, t: Double)(implicit su: SystemOfUnits) {
    /** Calculate new position in time = t1. */
    def integrate(t1: Double): MaterialPointState =
      MaterialPointState(
        r = r + speed :* (t1 - t),
        speed = speed,
        t = t1
      )
  }
  case class SolidBodyState(r: QuantityVector[Length], speed: Vector, t: Time)(implicit vs: VectorSpace[Vector, Time])
}
