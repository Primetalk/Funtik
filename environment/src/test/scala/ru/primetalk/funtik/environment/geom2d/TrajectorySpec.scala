package ru.primetalk.funtik.environment.geom2d

import org.specs2.{ScalaCheck, Specification}
import org.specs2.specification.core.SpecStructure
import ru.primetalk.funtik.environment.geom2d.lines2d.TwoPointsLine
import spire.implicits._
import Vector._
import DoublePrecision.DoubleCompareOps
import org.specs2.matcher.MatchResult

class TrajectorySpec extends Specification with ArbitraryVector2d with ScalaCheck with ArbitraryReasonableDouble { def is: SpecStructure =

    s2"""Specification of trajectory calculations
        |
        |Start at (5,5) and move at speed (3, 0).
        |  In 2 second position should be (11, 5)
        |$e1
        |
        |All vectors converted to polar and back should be the same.
        |$e2
        |
        |Parametric line segment should integrate after 1.0 to the second point.
        |$e3
        |
        |Parametric line segment should integrate after 0.5 to the center.
        |$e4
        |Parametric line segment should integrate after 0 to the first point.
        |$e5
        |
        |Vector multiplied by scalar should have length appropriately changed.
        |$e6
        |
        |Two circles should intersect in a known point
        |$twoCirclesIntersection
        |
        |pointToParam should return expected values
        |$pointToParam
        |
        |pointToParam should return expected values when moving clockwise
        |$pointToParamClockwise
        |""".stripMargin

  private def e1 = {
    val linear = Trajectory.Linear(Vector2d(5.0, 5.0), Vector2d(3.0, 0.0), 0)
    val (p, v) = linear.integrate(2.0)
    p must be equalTo Vector2d(11.0, 5.0)
  }
  private def e2 = {
    prop((v: Vector2d[Double]) =>
      (v.toPolar.toVector2d - v).length must be lessThan 0.01
    )
  }
  private def e3 = {
    prop((p1: Vector2d[Double], p2: Vector2d[Double], t0: ReasonableDouble) =>
      {
        val l = TwoPointsLine(p1, p2)
        val trajectory = l.toParametricLineDouble(t0.value)
        val (p, _) = trajectory.integrate(t0.value + 1)
        math.abs((p - p2).length)  must be lessThan 0.01
      }
    )
  }

  private def e4 = {
    prop((p1: Vector2d[Double], p2: Vector2d[Double], t0: ReasonableDouble) =>
    {
      val l = TwoPointsLine(p1, p2)
      val trajectory = l.toParametricLineDouble(t0.value)
      val (p, _) = trajectory.integrate(t0.value + 0.5)
      val center = (p1 + p2) :* 0.5
      math.abs((center - p).length)  must be lessThan 0.01
      ((center - p).length ~= 0.0) must be equalTo true
    }
    )
  }

  private def e5 = {
    prop((p1: Vector2d[Double], p2: Vector2d[Double], t0: ReasonableDouble) =>
    {
      val l = TwoPointsLine(p1, p2)
      val trajectory = l.toParametricLineDouble(t0.value)
      val (p, _) = trajectory.integrate(t0.value)
      math.abs((p - p1).length)  must be lessThan 0.01
    }
    )
  }
  private def e6 = {
    prop((v: Vector2d[Double], k: ReasonableDouble) =>
    {
      val kabs = math.abs(k.value)
      val vk = v :* kabs
      math.abs(v.length * kabs - vk.length)  must be lessThan 0.01
    }
    )
  }

  def twoCirclesIntersection: MatchResult[List[Double]] = {
    val c1 = Trajectory.Circular(Vector2d(0.0,0.0), 1.0, 0, 0, 1)
    val c2 = Trajectory.Circular(Vector2d(2.0,0.0), 1.0, 0, 0, 1)
    Trajectory.intersection(c1, c2).map(_._1).distinct must be equalTo List(0.0)
  }

  def pointToParam: MatchResult[Double] = {
    val t0 = 123.0
    val ph0 = 1.0
    val c1 = Trajectory.Circular(Vector2d(0.0,0.0), 1.0, t0, ph0, 1)
    c1.pointToParameter(Vector2d(0.0, 1.0)) must be equalTo (math.Pi/2 + t0 - ph0) and
      (c1.pointToParameter(Vector2d(-1.0, 0.0)) must be equalTo (math.Pi + t0 - ph0)) and
      (c1.pointToParameter(Vector2d(0.0, -1.0)) must be equalTo (3 * math.Pi/2 + t0 - ph0))
  }
  def pointToParamClockwise: MatchResult[Double] = {
    val t0 = 123.0
    val ph0 = 1.0
    val c1 = Trajectory.Circular(Vector2d(0.0,0.0), 1.0, t0, ph0, -1)
    math.abs(c1.pointToParameter(Vector2d(0.0, 1.0)) - (3*math.Pi/2 + t0 + ph0)) must be lessThan 0.01 and
      (c1.pointToParameter(Vector2d(-1.0, 0.0)) - (math.Pi + t0 + ph0) must be lessThan 0.01) and
      (c1.pointToParameter(Vector2d(0.0, -1.0)) - (math.Pi/2 + t0 + ph0) must be lessThan 0.01)
  }
}
