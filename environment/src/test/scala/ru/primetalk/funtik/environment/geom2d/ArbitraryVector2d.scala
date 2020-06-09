package ru.primetalk.funtik.environment.geom2d

import org.scalacheck.{Arbitrary, Gen}
import shapeless.tag
import shapeless.tag.@@

trait ArbitraryVector2d extends ArbitraryReasonableDouble {

  def arbVector2dInRect(bottomLeft: Vector2d[Double], topRight: Vector2d[Double]): Arbitrary[Vector2d[Double] ] = {
    val Vector2d(x1, y1) = bottomLeft
    val Vector2d(x2, y2) = topRight
    Arbitrary(
      for {
        x <- Gen.choose[Double](x1, x2)
        y <- Gen.choose[Double](y1, y2)
      } yield Vector2d(x, y)
    )
  }

  sealed trait DirectionTag
  implicit def arbDirection: Arbitrary[Vector2d[Double] @@ DirectionTag] =
    Arbitrary(
      arbReasonableDouble.arbitrary.
        map(theta =>
          tag[DirectionTag](Vector2dPolar(1, theta.value).toVector2d)
        )
    )

  implicit def arbVector2d: Arbitrary[Vector2d[Double]] =
    arbVector2dInRect(Vector2d(-1000.0, -1000.0), Vector2d(1000.0, 1000.0))
}
