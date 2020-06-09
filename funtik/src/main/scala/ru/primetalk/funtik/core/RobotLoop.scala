package ru.primetalk.funtik.core

import ru.primetalk.funtik.environment.geom2d.Vector2d
import ru.primetalk.funtik.environment.{Display, EnvironmentModel}
import ru.primetalk.funtik.environment.geom2d.Geom2dUtils._
//import spire.syntax.all._
//import Vector2d._
import spire.implicits._

sealed trait CellState

object CellState {

  case object Unknown extends CellState

  case object Obstacle extends CellState

  case object FreeCell extends CellState

}

trait RobotLoopT extends EnvironmentModel {

  case class InternalRobotState(
    target: Vector,
    currentPositionEstimate: Vector2d[Double],
    currentPositionTimeSinceStartMs: Long,
    currentSpeedEstimate: Vector2d[Double],
    map: Display[CellState])

  val initialInternalRobotState: InternalRobotState = InternalRobotState(
    target = Vector2d(10,10),
    currentPositionEstimate = Vector2d(0,0),
    currentPositionTimeSinceStartMs = 0L,
    currentSpeedEstimate = Vector2d(0,0),
    map = Display[CellState](Rectangle(Vector2d(-10,-10), Vector2d(21,21)))
  )
  def handleSensorDataIgnoreAll: (InternalRobotState, RobotSensorData) => (InternalRobotState, List[RobotCommand]) = {
    case (irs, _) => (irs, Nil)
  }
  def handleSensorDataRotateSimple: (InternalRobotState, RobotSensorData) => (InternalRobotState, List[RobotCommand]) = {
    case (
      irs,
      HitObstacle(_)
      ) =>

      (irs, List(SetSpeed(-1.0, -0.5)))
    case (irs, TimePassed(_)) => (irs, List(SetSpeed(1.0, 1.0)))
    case (irs, _) => (irs, Nil)
  }
  // stateFlatMap
  def handleSensorData: (InternalRobotState, RobotSensorData) => (InternalRobotState, List[RobotCommand]) = {
    case (
        InternalRobotState(
          target, currentPositionEstimate,
          currentPositionTimeSinceStartMs, currentSpeedEstimate, map),
        TimePassed(sinceStartMs)
        ) =>
      val newPositionEstimate = currentPositionEstimate + currentSpeedEstimate :* (sinceStartMs - currentPositionTimeSinceStartMs).toInt
      if (target == currentPositionEstimate) {
        (InternalRobotState(target, newPositionEstimate, sinceStartMs, Vector2d(0, 0), map), List(SetSpeed(0, 0)))
        // TODO: update map - add free cells along the bresenham line
      } else {
        val newSpeedDirection   = (target.toDouble - currentPositionEstimate).normalized
        val oldSpeedDirection   = currentSpeedEstimate.normalized
        val directionDifference = newSpeedDirection - oldSpeedDirection // x - is the direction difference, while y - speed difference

        (
          InternalRobotState(target, newPositionEstimate, sinceStartMs, currentSpeedEstimate, map),
          List(
            if (directionDifference._1 > 0) {
              SetSpeed(1 - 1 * directionDifference._1, 1)
            } else {
              SetSpeed(1, 1 - 1 * directionDifference._1)
            }
          )
        )
      }
    case (
        InternalRobotState(target, currentPositionEstimate, currentPositionTimeSinceStartMs, currentSpeedEstimate, map),
        HitObstacle(sinceStartMs)
        ) =>
      val newPositionEstimate = currentPositionEstimate + currentSpeedEstimate :* (sinceStartMs - currentPositionTimeSinceStartMs).toInt
      (InternalRobotState(target, newPositionEstimate, sinceStartMs, Vector2d(0, 0), map), Nil)
    case (internalRobotState:InternalRobotState, MagicalSpeedSensor(speed)) =>
      (internalRobotState.copy(currentSpeedEstimate = speed), Nil)
  }
}
