package ru.primetalk.funtik.environment.solid

import org.scalacheck.{Arbitrary, Gen}
import ru.primetalk.funtik.environment.geom2d.{ArbitraryVector2d, Vector2d}

trait ArbitraryMaterialParticle extends ArbitraryVector2d {
  import SolidBodyModel._
  implicit def arbMaterialParticleState: Arbitrary[MaterialParticleState] =
    Arbitrary(
      for {
        p <- Arbitrary.arbitrary[Vector2d[Double]]
        v <- Arbitrary.arbitrary[Vector2d[Double]]
        a <- Gen.choose[Double](-1000.0, 1000.0)// Arbitrary.arbitrary[Double]
        t <- Gen.choose[Double](1000.0, 1000000.0)
      } yield MaterialParticleState(p, v, a, t)
    )
}
