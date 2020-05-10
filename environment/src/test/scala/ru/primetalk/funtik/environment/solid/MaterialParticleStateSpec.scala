package ru.primetalk.funtik.environment.solid

import org.specs2.Specification
import SolidBodyModel._
import org.specs2.matcher.MatchResult
import ru.primetalk.funtik.environment.geom2d.{CollisionShape, Vector2d}

class MaterialParticleStateSpec extends Specification {
  def is = s2""" 
  MaterialParticleState should
    detect simple collision $simpleCollision
  """

  def simpleCollision: MatchResult[Seq[Axis]] = {
    val position = Vector2d(0.0, 0.0)
    val speed    = Vector2d(1.0, 0.0)
    val state    = MaterialParticleState(position, speed, 0)

    val squareLines = Seq(
      CollisionShape.LineSegment(Vector2d[Double](-40, -30), Vector2d[Double](39, -30)),
      CollisionShape.LineSegment(Vector2d[Double](39, -30), Vector2d[Double](39, 29)),
      CollisionShape.LineSegment(Vector2d[Double](39, 29), Vector2d[Double](-40, 29)),
      CollisionShape.LineSegment(Vector2d[Double](-40, 29), Vector2d[Double](-40, -30))
    )
    val result = squareLines.flatMap(state.detectNearestCollision)
    result must not be empty
  }
}
