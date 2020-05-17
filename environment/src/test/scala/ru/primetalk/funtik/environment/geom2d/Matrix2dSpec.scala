package ru.primetalk.funtik.environment.geom2d

import org.specs2.{ScalaCheck, Specification}
import org.specs2.specification.core.SpecStructure

class Matrix2dSpec extends Specification with ScalaCheck with ArbitraryMatrix2d {

  def is: SpecStructure = s2"""Matrix2d properties
    |
    | Forall square M 2x2 if D[M] != 0
    |   M * M^-1 = I
    | $propInvertMatrix
    |""".stripMargin


  private def propInvertMatrix = {

    import DoublePrecision._

    prop{ (m: Matrix2d[Double]) =>
      val (inv, determinant) = m.inverse

      determinant ~ 0.0 || m.*(inv) ~ Matrix2d.I
    }
  }
}
