package ru.primetalk.funtik.environment.geom2d

import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck

trait ArbitraryMatrix2d extends ScalaCheck {
  def doubleLimited: Arbitrary[Double] = Arbitrary(Gen.choose(-1000.0, 1000.0))
  implicit def arbMatrix2d: Arbitrary[Matrix2d[Double]] = Arbitrary(
    for {
      a <- doubleLimited.arbitrary
      b <- doubleLimited.arbitrary
      c <- doubleLimited.arbitrary
      d <- doubleLimited.arbitrary
    } yield Matrix2d(a, b, c, d)
  )
}
