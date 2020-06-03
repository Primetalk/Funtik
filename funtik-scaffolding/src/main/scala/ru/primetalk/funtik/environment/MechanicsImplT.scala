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
import ru.primetalk.funtik.environment.geom2d.DoublePrecision.DoubleCompareOps

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
          case WorldState(RobotEnvState(solidBodyState0), robotInternalState, worldPointMap) =>
            val minTimeOpt = findNearestCollisionTime(solidBodyState0, worldPointMap)
            val t1 = (wallTime :: minTimeOpt.toList).min // it's either collision or modelling duration.
            val t1ms = (t1 * su.time).millis
            val SolidBodyState(materialParticleAtT1, thetaAtT1) = solidBodyState0.integrate(t1)
            val (robotState1, commands1) = robotStrategy(robotInternalState, TimePassed(t1ms))
            // TOOD: issue GyroscopeInfo to robot ?
            val (robotStateAtWallTime, commands) = if(t1 !~  wallTime) {
              val (robotState1hit, commands2) = robotStrategy(robotState1, HitObstacle(t1ms))
              val (robotState2, commands3) = robotStrategy(robotState1hit, TimePassed(wallTimeMs))
              (robotState2, commands1 ::: commands2 ::: commands3)
            } else
              (robotState1, commands1)
            // we ignore all commands except the last one
            // because there is a natural delay between issuing command and seeing it's result.
            val newMaterialParticleState = commands.foldLeft(materialParticleAtT1)(applyCommand)

            val solidBodyState2 = SolidBodyState(newMaterialParticleState, thetaAtT1).integrate(wallTime)

            val newState =
              state.copy(
                robotEnvState = RobotEnvState(solidBodyState2),
                robotInternalState = robotStateAtWallTime,
              )
            State.pure((newState, defaultDuration))
        }

    }
  }

  private def findNearestCollisionTime[S](solidBodyState: SolidBodyState, worldPointMap: WorldPointMap) = {
    worldPointMapToCollisionShapes(worldPointMap).
      flatMap(solidBodyState.materialParticle.detectNearestCollision).
      map(_.t).
      minOption
  }

  def applyCommand(materialParticleState: MaterialParticleState, command: RobotCommand): MaterialParticleState = command match {
    case SetSpeed(left, right) =>
      val SolidBodyRotationParameters(tangentialVelocity, orthogonalAcceleration) = robot.convertTwoWheelsSpeedToSpeedAndOmega(left, right)
      materialParticleState.copy(speed = materialParticleState.speed.withLength(tangentialVelocity), orthogonalAcceleration = orthogonalAcceleration)
  }

  def worldPointMapToCollisionShapes(worldPointMap: WorldPointMap): List[CollisionShape[Double]] = {
    worldPointMap.pointsFiltered(identity).map{
      p =>
        CollisionShape.orthogonalSquare(p.toDouble, 1)
    }
  }
}