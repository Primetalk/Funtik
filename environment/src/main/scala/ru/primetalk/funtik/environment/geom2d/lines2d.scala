package ru.primetalk.funtik.environment.geom2d

import spire.implicits._
import Vector._

object lines2d {

  case class TwoPointsLine(p1: Vector2d[Double], p2: Vector2d[Double]) {
    /**
     * (x - x1)/(x2 - x1) = (y - y1)/(y2-y1)
     * x * (y2 - y1) - y * (x2 - x1) - x1 * (y2 - y1) + y1 * (x2 - x1) = 0
     * x * (y2 - y1) + y * (x1 - x2) + y1 * x2 - x1 * y2 = 0
     */
    def toEquationLine: EquationLine[Double] = {
      val x1 = p1.x
      val x2 = p2.x
      val y1 = p1.y
      val y2 = p2.y
      EquationLine(
        y2 - y1, x1 - x2, y1 * x2 - x1 * y2
      )
    }

    /** Important: p1 when t=0, p2 when t=1 */
    def toParametricLineDouble(t0: Double): Trajectory.Linear = {
      Trajectory.Linear(p1, p2 - p1, t0)
    }
  }
  /**
   * A line given by equation
   *
   * ax + by + c == 0
   *
   */
  case class EquationLine[Axis](a: Axis, b: Axis, c: Axis) {
    def r(implicit toDouble: Axis =:= Double): Axis = {
      val aa = toDouble(a)
      val bb = toDouble(b)
      val fromDouble = toDouble.flip
      fromDouble(math.sqrt(aa*aa + bb*bb))
    }
    def distance(p: Vector2d[Double])(implicit toDouble: Axis =:= Double): Double = {
      math.abs(a * p.x + b * p.y + c)/r
    }
  }


}
