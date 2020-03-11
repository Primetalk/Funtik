package ru.primetalk.funtik.environment.geom2d

import org.specs2.Specification
import org.specs2.specification.core.SpecStructure

class Geom2dUtilsSpec extends Specification { def is: SpecStructure =
  s2"""Specification of geometry utils
      |
      |  The line (0,0) -- (1,1) should
      |    contain two points
      |$e1
      |
      |  The line (0,0) -- (4,20) should
      |    contain 21 points
      |$e2
      |
      |  The line (0.6,0) -- (1.6, 1) should start at (1,0)
      |$e3
      |""".stripMargin
  private def e1 = Geom2dUtils.bresenhamLine(0.0, 0.0, 1.0, 1.0) must be equalTo List((0,0), (1,1))
  private def e2 = Geom2dUtils.bresenhamLine(0.0, 0.0, 4.0, 20.0) must haveSize(21)
  private def e3 = Geom2dUtils.bresenhamLine(0.6, 0.0, 1.6, 1.0).head must be equalTo  (1, 0)
}
