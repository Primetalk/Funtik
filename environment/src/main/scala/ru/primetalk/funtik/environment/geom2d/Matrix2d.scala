package ru.primetalk.funtik.environment.geom2d

/** It's a matrix:
 *  /     \
 *  | a b |
 *  | c d |
 *  \     /
 */
case class Matrix2d[Axis](a: Axis, b: Axis, c: Axis, d: Axis) {
  def determinant(implicit num: Numeric[Axis]): Axis = {
    import num.mkNumericOps
    a * d - b * c
  }

  def inverse(implicit num: Fractional[Axis]): (Matrix2d[Axis], Axis) = {
    import num.mkNumericOps
    val determinant1 = determinant
    val determinantInv = num.one / determinant1
    (Matrix2d(d, num.negate(b), num.negate(c), a).times(determinantInv), determinant1)
  }

  def times(s: Axis)(implicit num: Numeric[Axis]): Matrix2d[Axis] = {
    import num.mkNumericOps
    Matrix2d(s * a, s * b, s * c, s * d)
  }

  def *(v: Vector2d[Axis])(implicit num: Numeric[Axis]): Vector2d[Axis] = {
    import num.mkNumericOps
    Vector2d(a * v.x + b * v.y, c * v.x + d * v.y)
  }

  def *(other: Matrix2d[Axis])(implicit num: Numeric[Axis]): Matrix2d[Axis] = {
    import num.mkNumericOps
    Matrix2d(a * other.a + b * other.c, a * other.b + b * other.d,
             c * other.a + d * other.c, c * other.b + d * other.d)
  }

  def -(other: Matrix2d[Axis])(implicit num: Numeric[Axis]): Matrix2d[Axis] = {
    import num.mkNumericOps
    Matrix2d(a - other.a, b - other.b,
             c - other.c, d - other.d)
  }

}

object Matrix2d {
  def I[Axis](implicit num: Numeric[Axis]): Matrix2d[Axis] =
    Matrix2d(num.one, num.zero, num.zero, num.one)

  implicit class CompareMatrix2dDouble(m1: Matrix2d[Double]) {
    def ~(m2: Matrix2d[Double])(implicit doublePrecision: DoublePrecision): Boolean  = {
      math.abs((m1 - m2).determinant) < doublePrecision.epsilon
    }
  }

  def defaultEpsilonDouble: Double = 1e-10

}
