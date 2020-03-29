package ru.primetalk.funtik.core

import ru.primetalk.funtik.environment.geom2d.Geom2dUtils
import ru.primetalk.funtik.environment.{Display, EnvironmentModel}
import ru.primetalk.funtik.environment.geom2d.Geom2dUtils._
import ru.primetalk.funtik.environment.geom2d.Vector2dBasicSyntax._
import spire.syntax.all._
import ru.primetalk.funtik.environment.geom2d.Vector2dSyntax._

sealed trait CellState

object CellState {

  case object Unknown extends CellState

  case object Obstacle extends CellState

  case object FreeCell extends CellState

}

trait RobotLoop extends EnvironmentModel {

  case class InternalRobotState(
    target: Vector,
    currentPositionEstimate: Vector,
    currentPositionTimeSinceStartMs: Long,
    currentSpeedEstimate: Vector,
    map: Display[CellState])

  // stateFlatMap
  def handleSensorData: (InternalRobotState, RobotSensorData) => (InternalRobotState, List[RobotCommand]) = {
    case (
        InternalRobotState(
          target, currentPositionEstimate,
          currentPositionTimeSinceStartMs, currentSpeedEstimate, map),
        TimePassed(sinceStartMs)
        ) =>
      val newPositionEstimate = currentPositionEstimate // + currentSpeedEstimate * (sinceStartMs - currentPositionTimeSinceStartMs).toInt
      if (target == currentPositionEstimate) {
        (InternalRobotState(target, newPositionEstimate, sinceStartMs, (0, 0), map), List(SetSpeed(0, 0)))
        // TODO: update map - add free cells along the bresenham line
      } else {
        val newSpeedDirection   = (target - currentPositionEstimate).toDouble.normalized
        val oldSpeedDirection   = currentSpeedEstimate.toDouble.normalized
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
      val newPositionEstimate = currentPositionEstimate // currentSpeedEstimate * (sinceStartMs - currentPositionTimeSinceStartMs).toInt
      (InternalRobotState(target, newPositionEstimate, sinceStartMs, (0, 0), map), Nil)
  }
}
