package ru.primetalk.funtik.environment.solid

import org.specs2.{ScalaCheck, Specification}
import org.specs2.specification.core.SpecStructure
import ru.primetalk.funtik.environment.geom2d.{ArbitraryVector2d, Vector, Vector2d}
import spire.implicits._
import Vector._
import org.specs2.matcher.MatchResult
import org.specs2.scalacheck.ScalaCheckFunction1
import ru.primetalk.funtik.environment.geom2d.Trajectory.{Circular, Linear}

class SolidBodyModelSpec extends Specification with ArbitraryMaterialParticle with ScalaCheck { override def is: SpecStructure =
    s2"""
         |MaterialParticle should not jump
         |$e1
         |
         |Detect nearest collision should work for a simple case
         | $e2
         |""".stripMargin
  def e1: ScalaCheckFunction1[MaterialParticleState, MatchResult[Double]] = {
    prop((materialParticleState : MaterialParticleState) => {
      val trajectory = materialParticleState.trajectory
      val (position2, speed2) = trajectory.integrate(materialParticleState.t)
      (trajectory.t0 must be equalTo materialParticleState.t) and
        (math.abs(position2.length - materialParticleState.position.length) must be lessThan 0.01) and
        (math.abs(speed2.length - materialParticleState.speed.length) must be lessThan 0.01) and
        ((position2 - materialParticleState.position).length must be lessThan 0.01) and
        ((speed2 - materialParticleState.speed).length must be lessThan 0.01)
    }
    )
  }

  def e2 = ???
}
