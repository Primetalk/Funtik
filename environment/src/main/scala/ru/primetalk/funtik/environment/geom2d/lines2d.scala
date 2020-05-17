package ru.primetalk.funtik.environment.geom2d

import spire.implicits._
import Vector._
import spire.algebra.CModule
import DoublePrecision.DoubleCompareOps

object lines2d {
  /** ParametricLine:
   * {{{
   * r = r0 + v * t
   * }}}
   */
  case class ParametricLine[Axis](r0: Vector2d[Axis], v: Vector2d[Axis]) {
    def toEquationLine(implicit cmodule: CModule[Axis, Double], num: Numeric[Axis]): EquationLine[Axis] =
      toTwoPointsLine.toEquationLine
    def toTwoPointsLine(implicit cmodule: CModule[Axis, Double]): TwoPointsLine[Axis] =
      TwoPointsLine(r0, vectorSpaceForVector2d[Axis, Double].additive.combine(r0, v))
    /** A function that calculates parameter for a point on a line.
     * NB! it doesn't check if the point indeed on the line. If not - the result is undefined.*/
    def pointToParameter(p: Vector2d[Axis])(implicit cmodule: CModule[Axis, Double], num: Fractional[Axis]): Axis = {
      import num.mkOrderingOps
      import num.mkNumericOps
      val r = vectorSpaceForVector2d[Axis, Double].minus(p, r0)
      if(num.abs(v.x) > num.abs(v.y)) {
        r.x / v.x
      } else {
        r.y / v.y
      }
    }
  }

  case class TwoPointsLine[Axis](p1: Vector2d[Axis], p2: Vector2d[Axis]) {
    /**
     * (x - x1)/(x2 - x1) = (y - y1)/(y2-y1)
     * x * (y2 - y1) - y * (x2 - x1) - x1 * (y2 - y1) + y1 * (x2 - x1) = 0
     * x * (y2 - y1) + y * (x1 - x2) + y1 * x2 - x1 * y2 = 0
     */
    def toEquationLine(implicit num: Numeric[Axis]): EquationLine[Axis] = {
      import num.mkNumericOps
      val x1 = p1.x
      val x2 = p2.x
      val y1 = p1.y
      val y2 = p2.y
      EquationLine(
        y2 - y1, x1 - x2, y1 * x2 - x1 * y2
      )
    }

    /** Important: p1 when t=0, p2 when t=1 */
    def toParametricLineDouble(implicit num: Numeric[Axis]): ParametricLine[Axis] = {
      import num.mkNumericOps
      ParametricLine(p1, Vector2d(p2.x - p1.x, p2.y - p1.y))
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

  /** Finds intersection of two lines. If there is an intersection,
   * it returns `t1` and `t2` - parameter values for each line where they intersect.
   * It solves the following equation
   * r1 = r10 + v1 * t1 == r2 = r20 + v2 * t2
   * v1 * t1 - v2 * t2 == - (r10 - r20)
   * t = (t1, t2)  -- vector 2x1
   * v = (v1, -v2) -- matrix 2x2
   * v * t == r20 - r10
   * vInv = v ** -1
   * vInv * v * t == vInv * (r20-r10)
   * I        * t == vInv * (r20-r10)
   * t = vInv * (r20 - r10)
   */
  def intersection(l1: ParametricLine[Double], l2: ParametricLine[Double])(implicit doublePrecision: DoublePrecision): Option[(Double, Double)] = {
    val v1 = l1.v
    val v2 = l2.v
    val r10 = l1.r0
    val r20 = l2.r0
    val v = Matrix2d[Double](v1.x, -v2.x, v1.y, -v2.y)
    val (vInv, determinant) = v.inverse
    Option.when(determinant !~ 0.0) {
      val t = vInv * (r20 - r10)
      t.toTuple
    }
  }
}
