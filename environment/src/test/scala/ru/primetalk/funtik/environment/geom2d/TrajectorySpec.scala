package ru.primetalk.funtik.environment.geom2d

import org.specs2.Specification
import org.specs2.specification.core.SpecStructure

class TrajectorySpec extends Specification { def is: SpecStructure =

    s2"""Specification of trajectory calculations
        |
        |Start at (5,5) and move at speed (3, 0).
        |  In 2 second position should be (11, 5)
        |$e1
        |""".stripMargin

  private def e1 = {
    val linear = Trajectory.Linear(Vector2d(5.0, 5.0), Vector2d(3.0, 0.0), 0)
    val (p, v) = linear.integrate(2.0)
    p must be equalTo Vector2d(11.0, 5.0)
  }
}
