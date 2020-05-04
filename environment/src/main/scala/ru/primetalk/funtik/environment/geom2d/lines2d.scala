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
      Trajectory.Linear(p1, Vector2d(p2.x - p1.x, p2.y - p1.y), t0)
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

  val epsilon = 1e-10
  /** Finds intersection of two lines. If there is an intersection,
   * it returns `t1` and `t2` - parameter values for each line where they intersect.
   * It solves the following equation
   * r1 = r10 + v1 * t1 == r2 = r20 + v2 * t2
   * v1 * t1 - v2 * t2 == - (r10 - r20)
   * t = (t1, t2)
   * v = (v1, -v2)
   * vInv = v ** -1
   * t = vInv * (r20 - r10)
   */
  def intersection(l1: Trajectory.Linear, l2: Trajectory.Linear): Option[(Double, Double)] = {
    val v1 = l1.velocity
    val v2 = l2.velocity
    val r10 = l1.r0
    val r20 = l2.r0
    val v = Matrix2d[Double](v1.x, -v2.x, v1.y, -v2.y)
    val (vInv, d) = v.inverse
    if(math.abs(d) < epsilon)
      None
    else {
      val res = vInv * (r20 - r10)
      Some((res.x, res.y))
    }
  }
}
