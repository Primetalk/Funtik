package ru.primetalk.funtik.environment.geom2d

case class DoublePrecision(epsilon: Double = 1e-10)

object DoublePrecision {
  implicit val defaultDoublePrecision: DoublePrecision = DoublePrecision()

  implicit class DoubleCompareOps(d: Double){
    def ~(other: Double)(implicit doublePrecision: DoublePrecision): Boolean =
      math.abs(d - other) < doublePrecision.epsilon
    def !~(other: Double)(implicit doublePrecision: DoublePrecision): Boolean =
      !this.~(other)(doublePrecision)
    def >~(other: Double)(implicit doublePrecision: DoublePrecision): Boolean =
      d - other > doublePrecision.epsilon
    def <~(other: Double)(implicit doublePrecision: DoublePrecision): Boolean =
      d - other < -doublePrecision.epsilon
  }

}
