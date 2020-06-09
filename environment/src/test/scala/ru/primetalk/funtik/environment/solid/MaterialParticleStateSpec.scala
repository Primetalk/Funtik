package ru.primetalk.funtik.environment.solid

import org.specs2.{ScalaCheck, Specification}
import SolidBodyModel._
import org.scalacheck.Arbitrary
import org.specs2.matcher.MatchResult
import org.specs2.scalacheck.ScalaCheckFunction2
import spire.implicits._
import ru.primetalk.funtik.environment.geom2d.Geom2dUtils._
import ru.primetalk.funtik.environment.geom2d.{ArbitraryVector2d, CollisionShape, Trajectory, Vector2d, lines2d}
import ru.primetalk.funtik.environment.geom2d.CollisionShape.LineSegment
import shapeless.{tag => stag}
import stag.@@
import ru.primetalk.funtik.environment.geom2d.DoublePrecision.DoubleCompareOps
import ru.primetalk.funtik.environment.geom2d.lines2d.TwoPointsLine

class MaterialParticleStateSpec extends Specification with ScalaCheck with ArbitraryVector2d {

  val t0: Double = squants.time.Milliseconds(System.currentTimeMillis()) / su.time

  def is = s2""" 
  MaterialParticleState should
    detect collision right      ${axisCollision(Right)}
    detect collision left       ${axisCollision(Left)}
    detect collision up         ${axisCollision(Up)}
    detect collision down       ${axisCollision(Down)}
    detect collision up-right   ${axisCollision(Up + Right)}
    detect collision up-left    ${axisCollision(Up + Left)}
    detect collision down-left  ${axisCollision(Down + Left)}
    detect collision down-right ${axisCollision(Down + Right)}
    detect collision one line up  ${shouldCollideWith(Up,  upperBoundary, t0 + 29)}

    should detect collision for any point in rect $anyCollision

    circular trajectory should hit the surrounding rectangle $anyCollisionWithCircularTrajectory

    linear and circular trajectories should hit obstacle in approximately the same place $twoNearbyTrajectoriesShouldHitObstacleInNearbyPoints

  """

  private val upperBoundary: LineSegment[Double] = LineSegment(Vector2d(-40, 29), Vector2d(39, 29))

  private val bottomLeft: Vector2d[Double] = Vector2d(-40, -30)
  private val topRight: Vector2d[Double] = Vector2d(39, 29)

  private val rectEdges = CollisionShape.rectangle(bottomLeft, topRight)

  private def axisCollision(direction: Vector2d[Int]) = {
    val position = Vector2d(0.0, 0.0)
    val speed    = direction.toDouble
    val state    = MaterialParticleState(position, speed, 0, t0)
    val result   = rectEdges.flatMap(state.detectNearestCollision)
    result must not be empty
  }

  private def shouldCollideWith(direction: Vector2d[Int], line: LineSegment[Double], expectedTime: Double) = {
    val position = Vector2d(0.0, 0.0)
    val speed    = direction.toDouble
    val state    = MaterialParticleState(position, speed, 0, t0)
    val result   = state.detectNearestCollision(line).map(_.t)
    result must beSome (expectedTime)
  }

  sealed trait InRectTag

  implicit def arbInRectVector: Arbitrary[Vector2d[Double] @@ InRectTag] =
    Arbitrary(arbVector2dInRect(bottomLeft, topRight).arbitrary.map(stag[InRectTag](_)))

  def anyCollision: ScalaCheckFunction2[Vector2d[Double] @@ InRectTag, Vector2d[Double] @@ DirectionTag, MatchResult[Iterable[Any]]] = {
    prop { (start: Vector2d[Double] @@ InRectTag, direction: Vector2d[Double] @@ DirectionTag) =>
      val state    = MaterialParticleState(start, direction, 0, t0)
      val result   = rectEdges.flatMap(state.detectNearestCollision)
      val allStatesOnEdges = result.map(tr => state.integrate(tr.t))
      (result must not be empty) and
        (allStatesOnEdges should have size greaterThanOrEqualTo (1)) and
        forall(allStatesOnEdges) {
          stateOnEdge =>
            rectEdges.flatMap(stateOnEdge.detectNearestCollision) must beEmpty
        }
    }
  }

  def anyCollisionWithCircularTrajectoryDiscriminant: ScalaCheckFunction2[Vector2d[Double] @@ InRectTag, Vector2d[Double] @@ DirectionTag,
    MatchResult[Iterable[Axis]]] = {
    prop { (start: Vector2d[Double] @@ InRectTag, direction: Vector2d[Double] @@ DirectionTag) =>
      val state    = MaterialParticleState(start, direction, 0.000001, t0)
      val result   = rectEdges.map {
        edge =>
          val c1 @ Trajectory.Circular(_,_,_,_,_) = state.trajectory
          val LineSegment(p1, p2) = edge
          val l2 = lines2d.
            TwoPointsLine(p1, p2).
            toParametricLineDouble(0.0)

          val center = c1.center
          val r = c1.radius
          val TwoPointsLine(p10, p20) = l2.toTwoPointsLine
          val TwoPointsLine(Vector2d(x1, y1), Vector2d(x2, y2)) = lines2d.TwoPointsLine(p10 - center, p20 - center)
          val dx = x2 - x1
          val dy = y2 - y1
          val `dr^2` = dx * dx + dy * dy
          val D = x1 * y2 - x2 * y1
          val discriminant = r * r * `dr^2` - D * D
          discriminant
      }

      atLeastOnce(result)(r => (r >=~0) must beTrue)
    }
  }
  def anyCollisionWithCircularTrajectory: ScalaCheckFunction2[Vector2d[Double] @@ InRectTag, Vector2d[Double] @@ DirectionTag,
    MatchResult[Iterable[Axis]]] = {
    prop { (start: Vector2d[Double] @@ InRectTag, direction: Vector2d[Double] @@ DirectionTag) =>
      val state    = MaterialParticleState(start, direction, 0.01, t0)
      val result   = rectEdges.flatMap(state.detectNearestCollision).map(_.t)
      result must not be empty and
        atLeastOnce(result)(t => (t >=~ t0) must beTrue)
    }
  }

  def twoNearbyTrajectoriesShouldHitObstacleInNearbyPoints: ScalaCheckFunction2[
    Vector2d[Double] @@ InRectTag,
    Vector2d[Double] @@ DirectionTag,
    MatchResult[Any],
  ] = {
    prop(twoTrajectoriesProperty)
  }

  private def twoTrajectoriesProperty: (Vector2d[Axis] @@ InRectTag, Vector2d[Axis] @@ DirectionTag) => MatchResult[Any] = {
    (start: Vector2d[Axis] @@ InRectTag, direction: Vector2d[Axis] @@ DirectionTag) =>
      val state1 = MaterialParticleState(start, direction, 0.0, t0)
      val result1 = rectEdges.flatMap(state1.detectNearestCollision)
      val state2 = MaterialParticleState(start, direction, -0.001, t0)
      state2.trajectory
      val result2 = rectEdges.flatMap(state2.detectNearestCollision).sortBy(_.t)

      (result1 must not be empty) and
        (result2 must not be empty) and
        (math.abs(result1.head.t - result2.head.t) must be lessThan 20)
  }
}
