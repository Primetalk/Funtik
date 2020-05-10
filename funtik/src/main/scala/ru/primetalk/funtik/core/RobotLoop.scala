package ru.primetalk.funtik.core

import ru.primetalk.funtik.environment.geom2d.Vector2d
import ru.primetalk.funtik.environment.{Display, EnvironmentModel}
import ru.primetalk.funtik.environment.geom2d.Geom2dUtils._
import spire.syntax.all._

import scala.util.Random

sealed trait CellState

object CellState {

  case object Unknown extends CellState

  case object Obstacle extends CellState

  case object FreeCell extends CellState

}

trait RobotLoopT extends EnvironmentModel {

  case class InternalRobotState(
    target: Vector,
    currentPositionEstimate: Vector,
    currentPositionTimeSinceStartMs: Long,
    currentSpeedEstimate: Vector,
    map: Display[CellState])

  val initialInternalRobotState: InternalRobotState = InternalRobotState(
    target = Vector2d(10,10),
    currentPositionEstimate = Vector2d(0,0),
    currentPositionTimeSinceStartMs = 0L,
    currentSpeedEstimate = Vector2d(0,0),
    map = Display[CellState](Rectangle(Vector2d(-10,-10), Vector2d(21,21)))
  )
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
        (InternalRobotState(target, newPositionEstimate, sinceStartMs, Vector2d(0, 0), map), List(SetSpeed(0, 0)))
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
        HitObstacle()
        ) =>
      val newPositionEstimate = currentPositionEstimate // currentSpeedEstimate * (sinceStartMs - currentPositionTimeSinceStartMs).toInt
      val x = Random.nextInt(2)
      val y = Random.nextInt(2)
      val newSpeed = SetSpeed(x, y)
      (InternalRobotState(target, newPositionEstimate, currentPositionTimeSinceStartMs, Vector(0, 0), map), List(newSpeed))
    case other => throw new IllegalArgumentException(s"Unsupported sensor data")
  }
}
