package ru.primetalk.funtik.core

import ru.primetalk.funtik.environment.EnvironmentModel
import ru.primetalk.funtik.environment.geom2d.Geom2dUtils._

trait RobotLoop extends EnvironmentModel {
  case class InternalRobotState
  (
    target: Vector,
    currentPositionEstimate: Vector,
    currentPositionTimeSinceStartMs: Long,
    currentSpeedEstimate: Vector)

  def handleSensorData: (InternalRobotState, RobotSensorData) => (InternalRobotState, Option[RobotCommand])  = {
    case (InternalRobotState(target, currentPositionEstimate, currentPositionTimeSinceStartMs, currentSpeedEstimate), TimePassed(sinceStartMs)) =>
      val newPositionEstimate = currentSpeedEstimate * (sinceStartMs - currentPositionTimeSinceStartMs).toInt
      if(target == currentPositionEstimate) {
        (InternalRobotState(target, newPositionEstimate, sinceStartMs, currentSpeedEstimate), Some(SetSpeed(0,0)))
      } else {

        val newSpeedDirection = (target - currentPositionEstimate).toDouble.normalized
        val oldSpeedDirection = currentSpeedEstimate.toDouble.normalized
        val directionDifference = newSpeedDirection - oldSpeedDirection // x - is the direction difference, while y - speed difference

        (InternalRobotState(target, newPositionEstimate, sinceStartMs, currentSpeedEstimate),
          Some(if(directionDifference._1 > 0) {
            SetSpeed(1 - directionDifference._1, 1)
          } else {
            SetSpeed(1, 1 - directionDifference._1)
          })
        )
      }
    case (InternalRobotState(target, currentPositionEstimate, currentPositionTimeSinceStartMs, currentSpeedEstimate), HitObstacle(sinceStartMs)) =>
      val newPositionEstimate = currentSpeedEstimate * (sinceStartMs - currentPositionTimeSinceStartMs).toInt
      (InternalRobotState(target, newPositionEstimate, sinceStartMs, (0,0)), None)
  }
}
