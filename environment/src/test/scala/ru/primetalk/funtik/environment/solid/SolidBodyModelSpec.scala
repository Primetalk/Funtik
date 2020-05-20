package ru.primetalk.funtik.environment.solid

import org.specs2.{ScalaCheck, Specification}
import org.specs2.specification.core.SpecStructure
import ru.primetalk.funtik.environment.geom2d.{Vector, Vector2d}
import spire.implicits._
import Vector._
import org.specs2.matcher.MatchResult
import org.specs2.scalacheck.{ScalaCheckFunction1, ScalaCheckFunction3}
import ru.primetalk.funtik.environment.solid.SolidBodyModel.su
import shapeless.tag.@@

class SolidBodyModelSpec extends Specification with ArbitraryMaterialParticle with ScalaCheck { override def is: SpecStructure =
    s2"""
         |MaterialParticle should not jump
         |$e1
         |
         |Nearby trajectories should integrate to nearby points
         |$nearbyTrajectoriesShouldIntegrateToNearbyPoints
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

  def nearbyTrajectoriesShouldIntegrateToNearbyPoints: ScalaCheckFunction1[MaterialParticleState, MatchResult[Double]] = {
    prop( (materialParticleState : MaterialParticleState) =>
    {
      val s1 = materialParticleState.copy(orthogonalAcceleration = 0)
      val s2 = materialParticleState.copy(orthogonalAcceleration = 0.01)
      val s11 = s1.integrate(s1.t + 1.0)
      val s21 = s2.integrate(s2.t + 1.0)
      (s11.position - s21.position).length should be lessThan 0.01
    })
  }
}
