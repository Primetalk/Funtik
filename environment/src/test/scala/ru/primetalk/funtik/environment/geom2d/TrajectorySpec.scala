package ru.primetalk.funtik.environment.geom2d

import org.specs2.{ScalaCheck, Specification}
import org.specs2.specification.core.SpecStructure

class TrajectorySpec extends Specification with ArbitraryVector2d with ScalaCheck { def is: SpecStructure =

    s2"""Specification of trajectory calculations
        |
        |Start at (5,5) and move at speed (3, 0).
        |  In 2 second position should be (11, 5)
        |$e1
        |
        |All vectors converted to polar and back should be the same.
        |$e2
        |""".stripMargin

  private def e1 = {
    val linear = Trajectory.Linear(Vector2d(5.0, 5.0), Vector2d(3.0, 0.0), 0)
    val (p, v) = linear.integrate(2.0)
    p must be equalTo Vector2d(11.0, 5.0)
  }
  private def e2 = {
    import spire.implicits._
    import Vector._
    prop((v: Vector2d[Double]) =>
      (v.toPolar.toVector2d - v).length must be lessThan 0.01
    )
  }
}
