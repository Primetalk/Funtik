package ru.primetalk.funtik.environment

import ru.primetalk.funtik.environment.generator.utils.Random.RandomStateValue

import scala.concurrent.duration.{Duration, DurationInt, FiniteDuration}
import ru.primetalk.funtik.environment.generator.{BSPTree, Tree}
import ru.primetalk.funtik.environment.geom2d.Geom2dUtils._
import cats.data.State
import ru.primetalk.funtik.environment.geom2d.{CollisionShape, Vector2d}
import ru.primetalk.funtik.environment.solid.{SolidBodyRotationParameters, _}
import ru.primetalk.funtik.environment.solid.SolidBodyModel._
import squants.time.Milliseconds

trait MechanicsImplT extends EnvironmentModel with RobotDefinitionT {

  class MechanicsImpl[S](val robotStrategy: RobotStrategy[S], val initialRobotState: S) extends ModelMechanics[S] {

    private def toPoints: Tree[Rectangle] => List[Vector2d[Int]] =
      Tree.eliminate[Rectangle, List[Position]](_.edges)(_ reverse_::: _)

    def generateDisplay(rect: Rectangle): State[RandomStateValue, Display[Boolean]] =
      BSPTree()(rect).map(toPoints).map { points =>
        Display.showPoints(points, true, false)
      }

    val defaultDuration: FiniteDuration = 40.milliseconds

    /** the returned Duration is the next event */
    override def start(wallTimeMs: Long, internalState0: S): State[RandomStateValue, (WorldState[S], Duration)] = {
      val rect = Rectangle(Vector2d(-40, -30), Vector2d(80, 60))
      generateDisplay(rect).map { display =>
        (
          WorldState[S](
            RobotEnvState(SolidBodyState(
              MaterialParticleState(
                position = Vector2d(10.0, 10.0),
                speed = Vector2d(0.0, 2.0),
                orthogonalAcceleration = -0.3,
                t = Milliseconds(wallTimeMs) / su.time
              ),
              theta = 0.0)),
            internalState0,
            display
          ),
          defaultDuration
        )
      }
    }

    /**
     * Receives an event from scaffolding (like real timer, key press, mouse click).
     * Despite that we try to perform deterministic modelling, we still need
     * to generate white noise for signals. Hence, Random state.
     */
    override def handleEvent(
                              state: WorldState[S],
                              e: ModellingEvent
                            ): State[RandomStateValue, (WorldState[S], Duration)] = e match {
      case ScaffoldingTimePassed(wallTimeMs) =>
        val wallTime = Milliseconds(wallTimeMs) / su.time
        state match {
          case WorldState(RobotEnvState(solidBodyState), robotInternalState, worldPointMap) =>
            val points = worldPointMapToCollisionShapes(worldPointMap)
            val minTimeOpt = points.
              flatMap(solidBodyState.materialParticle.detectNearestCollision).
              map(_.t).
              minOption
            val t1 = minTimeOpt.getOrElse(wallTime)
            val SolidBodyState(materialParticle1, theta1) = solidBodyState.integrate(t1)
            val t1ms = (t1 * su.time).millis
            val (rst1, commands1) = robotStrategy(robotInternalState, TimePassed(t1ms))
            // TOOD: issue GyroscopeInfo to robot ?
            val (rst12, commands) = if(minTimeOpt.isDefined) {
              val (rst2, commands2) = robotStrategy(rst1, HitObstacle(t1ms))
              val (rst3, commands3) = robotStrategy(rst2, TimePassed(wallTimeMs))
              (rst3, commands1 ::: commands2 ::: commands3)
            } else
              (rst1, commands1)
            // we ignore all commands except the last one
            // because there is a natural delay between issueing command and seeing it's result.
            val setSpeedOpt = commands.lastOption.collect{ case s@SetSpeed(_, _) => s }
            val newMaterialParticleState = setSpeedOpt.map{ case SetSpeed(left, right) =>
              val SolidBodyRotationParameters(tangentialVelocity, orthogonalAcceleration) = robot.convertTwoWheelsSpeedToSpeedAndOmega(left, right)
              materialParticle1.copy(speed = materialParticle1.speed.withLength(tangentialVelocity), orthogonalAcceleration = orthogonalAcceleration)
            }.getOrElse(materialParticle1)

            val newState =
              state.copy(
                robotEnvState = RobotEnvState(SolidBodyState(newMaterialParticleState, theta1)),
                robotInternalState = rst12,
              )
            State.pure((newState, defaultDuration))
        }

    }
  }

  def worldPointMapToCollisionShapes(worldPointMap: WorldPointMap): List[CollisionShape[Double]] = {
    worldPointMap.pointsFiltered(identity).map{
      p =>
        CollisionShape.orthogonalSquare(p.toDouble, 1)
    }
  }
}