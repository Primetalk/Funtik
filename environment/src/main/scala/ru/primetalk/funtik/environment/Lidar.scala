package ru.primetalk.funtik.environment

import ru.primetalk.funtik.environment.geom2d.{CollisionShape, Trajectory, Vector2d}

object Lidar {

  type LidarSamples = Array[Double]

  val minDistance = 0.0
  /** Reads n lidar distances that are visible from position.
   * Starts reading from angle theta.
   */
  def read(n: Int, position: Vector2d[Double], theta: Double, environment: List[CollisionShape[Double]]): LidarSamples = {
    (0 until n).toArray.
      flatMap{
        i =>
          val theta2 = theta + i * 2 * math.Pi / n
          val trajectory = Trajectory.Linear(position, Vector2d.direction(theta2), 0.0)
          environment.flatMap{ collisionShape =>
            Trajectory.detectNearestCollision(trajectory, collisionShape).
              map(_.t).
              filter(_ > minDistance)
          }
      }
  }
}
