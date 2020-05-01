package ru.primetalk.funtik.environment.geom2d

/** It's a matrix:
 *  /     \
 *  | a b |
 *  | c d |
 *  \     /
 */
case class Matrix2d[Axis](a: Axis, b: Axis, c: Axis, d: Axis) {

  def inverse(implicit num: Fractional[Axis]): (Matrix2d[Axis], Axis) = {
    import num.mkNumericOps
    val determinant = a * d - b * c
    val determinantInv = num.one / determinant
    (Matrix2d(d, num.negate(b), num.negate(c), a).times(determinantInv), d)
  }

  def times(s: Axis)(implicit num: Numeric[Axis]): Matrix2d[Axis] = {
    import num.mkNumericOps
    Matrix2d(s * a, s * b, s * c, s * d)
  }

  def *(v: Vector2d[Axis])(implicit num: Numeric[Axis]): Vector2d[Axis] = {
    import num.mkNumericOps
    Vector2d(a * v.x + b * v.y, c * v.x + d * v.y)
  }

}
