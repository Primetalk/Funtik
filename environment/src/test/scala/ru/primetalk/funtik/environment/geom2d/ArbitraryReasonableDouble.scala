package ru.primetalk.funtik.environment.geom2d

import org.scalacheck.{Arbitrary, Gen}

/**
 * Our default precision is 1e-5. Double has 15 significant digits.
 * So we should generate doubles in the range of at most +-1e+10
 *
 * Here we decided to generate smaller range of only +-1e+5.
 *
 * @see [[ru.primetalk.funtik.environment.geom2d.DoublePrecision]]
 */
trait ArbitraryReasonableDouble {
  case class ReasonableDouble(value: Double)
  implicit def arbReasonableDouble: Arbitrary[ReasonableDouble] =
    Arbitrary(Gen.choose[Double](-1e+5, 1e+5).map(ReasonableDouble))
}
