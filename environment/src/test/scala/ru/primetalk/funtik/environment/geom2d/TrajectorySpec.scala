package ru.primetalk.funtik.environment.geom2d

import org.specs2.{ScalaCheck, Specification}
import org.specs2.specification.core.SpecStructure
import ru.primetalk.funtik.environment.geom2d.lines2d.TwoPointsLine
import spire.implicits._
import Vector._
import DoublePrecision.DoubleCompareOps
import org.specs2.matcher.MatchResult
import ru.primetalk.funtik.environment.geom2d.CollisionShape.LineSegment

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
        |
        |A certain circular trajectory should cross line segment in the known point
        |$circularIntersectionCase
        |
        |Linear trajectory should intersect upper boundary at known point
        |$linearIntersectionCase2
        |
        |Circular trajectory should intersect upper boundary at known point
        |$circularIntersectionCase2
        |
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
    Trajectory.intersection(c1, c2).map(_._1.t).distinct must be equalTo List(0.0)
  }

  def pointToParam: MatchResult[Double] = {
    val t0 = 123.0
    val ph0 = 1.0
    val c1 = Trajectory.Circular(Vector2d(0.0,0.0), 1.0, t0, ph0, 2)
    c1.pointToParameter(Vector2d(0.0, 1.0)).t must be equalTo (math.Pi/4 + t0 - ph0 / 2) and
      (c1.pointToParameter(Vector2d(-1.0, 0.0)).t must be equalTo (math.Pi/2 + t0 - ph0 / 2)) and
      (c1.pointToParameter(Vector2d(0.0, -1.0)).t must be equalTo (3 * math.Pi/4 + t0 - ph0 / 2))
  }
  def pointToParamClockwise: MatchResult[Double] = {
    val t0 = 123.0
    val ph0 = math.Pi
    val c1 = Trajectory.Circular(Vector2d(0.0,0.0), 1.0, t0, ph0, -2)
    math.abs(c1.pointToParameter(c1.r0).t - t0) must be lessThan 0.01 and
      ((c1.r0 - Vector2d(-1.0, 0.0)).length must be lessThan 0.01) and
      (c1.pointToParameter(Vector2d(-1.0, 0.0)).t - (t0 + (math.Pi + ph0)/2) must be lessThan 0.01) //and
      (c1.pointToParameter(Vector2d(0.0, -1.0)).t - (math.Pi/2 + t0 + ph0) must be lessThan 0.01)
  }

  def circularIntersectionCase: MatchResult[List[Double]] = {
    val t0 = 0
    val ls = LineSegment[Double](Vector2d(100, 1), Vector2d(-100, 1))
    val l2 = Trajectory.Linear(ls.p1, ls.p2 - ls.p1, t0)
    val c1 = Trajectory.Circular(center = Vector2d(100, 0), radius = 100, t0 = t0, phase0 = math.Pi, omega = -0.01)
    val intersections = c1.intersect(l2).map(_._1.t)
    intersections must be equalTo List(1.0000166674167588, 313.15924869156254)
  }

  val upperBoundary: LineSegment[Double] = LineSegment[Double](Vector2d(39, 29), Vector2d(-40, 29))

  def linearIntersectionCase2: MatchResult[List[Vector2d[Double]]] = {

    val t0 = 0
    val l1 = Trajectory.Linear(Vector2d(0.0, 0), Vector2d(0.0,1), t0)
    val l2 = lines2d.
      TwoPointsLine(upperBoundary.p1, upperBoundary.p2).
      toParametricLineDouble(0.0)

    val intersections = Trajectory.intersectionPoints(l1, l2)
    intersections must be equalTo List(Vector2d(0.0, 29))
  }
  def circularIntersectionCase2: MatchResult[Any] = {
    val t0 = 0
    val l2 = lines2d.
      TwoPointsLine(upperBoundary.p1, upperBoundary.p2).
      toParametricLineDouble(0.0)
    val r = 1000
    val c1 = Trajectory.Circular(center = Vector2d(r, 0), radius = r, t0 = t0, phase0 = math.Pi, omega = - 1.0 / r)
    val intersections = Trajectory.intersectionPoints(c1, l2).minByOption(_.x)
    val expectedIntersectionPoint = Vector2d(0.42058844732093803, 29)
    intersections must be equalTo Some(expectedIntersectionPoint) and
      (c1.pointToParameter(expectedIntersectionPoint).t must be equalTo 29.004066372440462)
  }
}
