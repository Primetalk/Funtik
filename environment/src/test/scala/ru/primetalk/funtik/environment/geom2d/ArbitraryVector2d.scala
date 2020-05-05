package ru.primetalk.funtik.environment.geom2d

import org.scalacheck.{Arbitrary, Gen}

trait ArbitraryVector2d {
  implicit def arbVector2d: Arbitrary[Vector2d[Double]] =
    Arbitrary(
      for {
        x <- Gen.choose[Double](-1000.0, 1000.0)
        y <- Gen.choose[Double](-1000.0, 1000.0)// Arbitrary.arbitrary[Double]
      } yield Vector2d(x,y)
    )
}
