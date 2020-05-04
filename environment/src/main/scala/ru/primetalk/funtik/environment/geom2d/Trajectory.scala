package ru.primetalk.funtik.environment.geom2d

import spire.implicits._
import Vector._
import ru.primetalk.funtik.environment.geom2d.lines2d.{EquationLine, TwoPointsLine}
//import spire.algebra.CModule

sealed trait Trajectory {
  /** Returns position and velocity at the given moment. */
  def integrate(t: Double): (Vector2d[Double], Vector2d[Double])
  /** Returns parameter value for the point.
   * If point is not on the trajectory, the result is undefined. */
  def pointToParameter(p: Vector2d[Double]): Double
}

object Trajectory {
  /** Linear trajectory is given by the following equation:
   * {{{
   * r = r0 + v * (t - t0)
   * }}}
   */
  case class Linear(r0: Vector2d[Double], velocity: Vector2d[Double], t0: Double) extends Trajectory {
    def toEquationLine: EquationLine[Double] =
      toTwoPointsLine.toEquationLine
    def toTwoPointsLine: TwoPointsLine =
      TwoPointsLine(r0, r0 + velocity)
    /** A function that calculates parameter for a point on a line.
     * NB! it doesn't check if the point indeed on the line. If not - the result is undefined.*/
    def pointToParameter(p: Vector2d[Double]): Double = {
      val r = p - r0
      if(math.abs(velocity.x) > math.abs(velocity.y)) {
        r.x / velocity.x
      } else {
        r.y / velocity.y
      }
    }

    override def integrate(t: Double): (Vector2d[Double], Vector2d[Double]) =
      (r0 + (velocity :* (t - t0)), velocity)
  }

  case class Circular(center: Vector2d[Double], radius: Double, t0: Double, phase0: Double, omega: Double) extends Trajectory {
    override def integrate(t: Double): (Vector2d[Double], Vector2d[Double]) = {
      val phase = (t - t0) * omega - phase0
      (
        center + Vector2dPolar(radius, phase).toVector2d,
        Vector2dPolar(radius * omega, phase + math.Pi / 2).toVector2d
        )
    }

    def updateTrajectory(t: Double): Circular = {
      val (p, _) = integrate(t)
      val phase = (p - center).toPolar.theta
      this.copy(t0 = t, phase0 = phase)
    }
    /** Returns parameter value for the point.
     * If point is not on the trajectory, the result is undefined. */
    override def pointToParameter(p: Vector2d[Double]): Double = {
      val phase = (p - center).toPolar.theta
      val positiveDeltaPhase = (2 * math.Pi + phase - phase0) % (2 * math.Pi)
      positiveDeltaPhase / omega
    }
  }
}