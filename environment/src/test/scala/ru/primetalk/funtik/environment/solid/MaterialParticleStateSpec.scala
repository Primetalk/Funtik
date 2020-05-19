package ru.primetalk.funtik.environment.solid

import org.specs2.{ScalaCheck, Specification}
import SolidBodyModel._
import org.scalacheck.Arbitrary
import org.specs2.matcher.MatchResult
import org.specs2.scalacheck.ScalaCheckFunction2
import spire.implicits._
import ru.primetalk.funtik.environment.geom2d.Geom2dUtils._
import ru.primetalk.funtik.environment.geom2d.{ArbitraryVector2d, CollisionShape, Vector2d}
import ru.primetalk.funtik.environment.geom2d.CollisionShape.LineSegment
import shapeless.{tag => stag}
import stag.@@

class MaterialParticleStateSpec extends Specification with ScalaCheck with ArbitraryVector2d {

  val t0: Double = squants.time.Milliseconds(System.currentTimeMillis())/su.time

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

    should detect collision for any point in rect $anyCollision

    detect collision one line up  ${shouldCollideWith(Up,  upperBoundary, t0 + 29)}
  """

  private val upperBoundary: LineSegment[Double] = LineSegment(Vector2d(-40, 29), Vector2d(39, 29))

  private val bollomLeft: Vector2d[Double] = Vector2d(-40, -30)
  private val topRight: Vector2d[Double] = Vector2d(39, 29)

  private val rectEdges = CollisionShape.rectangle(bollomLeft, topRight)

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
    val result   = state.detectNearestCollision(line)
    result must beSome (expectedTime)
  }

  sealed trait InRectTag

  implicit def arbInRectVector: Arbitrary[Vector2d[Double] @@ InRectTag] =
    Arbitrary(arbVector2dInRect(bollomLeft, topRight).arbitrary.map(stag[InRectTag](_)))

  def anyCollision: ScalaCheckFunction2[Vector2d[Double] @@ InRectTag,
    Vector2d[Double] @@ DirectionTag,
    MatchResult[Seq[Axis]]] = {
    prop { (start: Vector2d[Double] @@ InRectTag, direction: Vector2d[Double] @@ DirectionTag) =>
      val state    = MaterialParticleState(start, direction, 0, t0)
      val result   = rectEdges.flatMap(state.detectNearestCollision)
      result must not be empty
    }
  }

}
