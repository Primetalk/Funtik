package ru.primetalk.funtik.environment

import ru.primetalk.funtik.environment.generator.utils.Random.{RandomState, RandomStateValue}

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

  sealed trait RobotEnvCommand
  object RobotEnvCommand {
    case class SetWheelSpeed(left: Double, right: Double) extends RobotEnvCommand
    case class IntegrateUpToTime(t: Double) extends RobotEnvCommand
  }

  def handleCommandOnEnvState(robotEnvState: RobotEnvState, robotEnvCommand: RobotEnvCommand): RobotEnvState = robotEnvCommand match {
    case RobotEnvCommand.SetWheelSpeed(left, right) =>
      val SolidBodyRotationParameters(tangentialVelocity, orthogonalAcceleration) = robot.convertTwoWheelsSpeedToSpeedAndOmega(left, right)
      robotEnvState.handleCommand(MaterialParticleStateCommand.
        SetSpeedAndAcceleration(
          speed = robotEnvState.solidBodyState.materialParticle.speed.withLength(tangentialVelocity),
          orthogonalAcceleration = orthogonalAcceleration,
        )
      )
    case RobotEnvCommand.IntegrateUpToTime(t) =>
      robotEnvState.handleCommand(MaterialParticleStateCommand.IntegrateUpToTime(t))
  }

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
                            ): RandomState[(WorldState[S], Duration)] = e match {
      case ScaffoldingTimePassed(wallTimeMs) =>
        val wallTime = Milliseconds(wallTimeMs) / su.time
        val newState = integrate(state, Nil, wallTime)
        State.pure((newState, defaultDuration))
    }
    def integrate(state: WorldState[S], queue: List[Either[Either[MaterialParticleStateCommand, RobotCommand], RobotSensorData]], targetTime: Double): WorldState[S] = queue match {
      case util.Right(robotSensorData) :: tail =>
        val (robotState1, commands) = robotStrategy(state.robotInternalState, robotSensorData)
        val commandsR = commands.map(cmd => util.Left(util.Right(cmd)))
        integrate(state.copy(
          robotInternalState = robotState1,
        ), tail ::: commandsR, targetTime)
      case util.Left(either) :: tail =>
        integrate(handleCommand(state, either), tail, targetTime)
      case Nil =>
        if(state.robotEnvState.solidBodyState.materialParticle.t ~= targetTime) {
          state
        } else {
          val minTimeOpt = findNearestCollisionTime(state.robotEnvState.solidBodyState, state.worldPointMap)
          val t1 = (targetTime :: minTimeOpt.toList).min // it's either collision or modelling duration.
          val t1ms = (t1 * su.time).millis
          val listOfCommands: List[Either[Either[MaterialParticleStateCommand, RobotCommand], RobotSensorData]] =  List(
            util.Left(util.Left(MaterialParticleStateCommand.IntegrateUpToTime(t1))),
            util.Right(TimePassed(t1ms)),
          )
          val listOfCommandsWithHitObstacleIfNecessary: List[Either[Either[MaterialParticleStateCommand, RobotCommand], RobotSensorData]] =
            if(t1 == targetTime)
              listOfCommands
            else
              listOfCommands ::: List(
                util.Left(util.Left(MaterialParticleStateCommand.SetSpeedAndAcceleration(Vector2d(0,0), 0.0))),
                util.Right(HitObstacle(t1ms)),
              )
          integrate(state, listOfCommandsWithHitObstacleIfNecessary, targetTime)
        }
    }

    def handleCommand(state: WorldState[S], cmd: Either[MaterialParticleStateCommand, RobotCommand]): WorldState[S] = cmd match {
      case util.Left(materialParticleStateCommand) =>
        state.copy(robotEnvState = state.robotEnvState.handleCommand(materialParticleStateCommand))
      case util.Right(SetSpeed(left, right)) =>
        val SolidBodyRotationParameters(speed, accel) = robot.convertTwoWheelsSpeedToSpeedAndOmega(left, right)
        val v = state.robotEnvState.solidBodyState.materialParticle.speed.withLength(speed)
        handleCommand(state, util.Left(MaterialParticleStateCommand.SetSpeedAndAcceleration(v, accel)))
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
