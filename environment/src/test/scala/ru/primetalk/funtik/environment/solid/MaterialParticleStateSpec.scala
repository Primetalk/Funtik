package ru.primetalk.funtik.environment.solid

import org.specs2.Specification
import SolidBodyModel._
import spire.implicits._
import ru.primetalk.funtik.environment.geom2d.Geom2dUtils._
import ru.primetalk.funtik.environment.geom2d.{ Vector, Vector2d }
import ru.primetalk.funtik.environment.geom2d.CollisionShape.LineSegment

class MaterialParticleStateSpec extends Specification {

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

    detect collision one line up  ${shouldCollideWith(Up,  upperBoundary, 29)}
  """

  private val upperBoundary: LineSegment[Double] = LineSegment(Vector2d(-40, 29), Vector2d(39, 29))

  private val squareLines = Seq(
    LineSegment(Vector2d[Double](-40, -30), Vector2d[Double](39, -30)),
    LineSegment(Vector2d[Double](39, -30), Vector2d[Double](39, 29)),
    upperBoundary,
    LineSegment(Vector2d[Double](-40, -30), Vector2d[Double](-40, 29))
  )

  private def axisCollision(direction: Vector2d[Int]) = {
    val position = Vector2d(0.0, 0.0)
    val speed    = direction.toDouble
    val state    = MaterialParticleState(position, speed, 0)
    val result   = squareLines.flatMap(state.detectNearestCollision)
    result must not be empty
  }

  private def shouldCollideWith(direction: Vector2d[Int], line: LineSegment[Double], expectedTime: Double) = {
    val position = Vector2d(0.0, 0.0)
    val speed    = direction.toDouble
    val state    = MaterialParticleState(position, speed, 0)
    val result   = state.detectNearestCollision(line)
    result must beSome (expectedTime)
  }
}
