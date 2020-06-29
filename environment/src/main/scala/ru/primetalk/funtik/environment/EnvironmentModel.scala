package ru.primetalk.funtik.environment

import cats.data.State
import ru.primetalk.funtik.environment.Lidar.LidarSamples
import ru.primetalk.funtik.environment.generator.utils.Random.RandomStateValue
import ru.primetalk.funtik.environment.geom2d.{CollisionShape, Vector2d, Vector3d}
import ru.primetalk.funtik.environment.solid._

import scala.concurrent.duration.Duration
// Environment should be able to represent the situation for "bring me the red ball from my room"-task.
// There should be rooms connected via other rooms/corridors.
trait EnvironmentModel {
  case class Place(name: String)
  val redBall: EnvironmentObject = EnvironmentObject("red ball")
  val myRoom: Place = Place("my room")
  val room2: Place = Place("room2")
  val currentRoom: Place = room2

  case class Environment(adjacency: List[(Place, Place)], contents: Map[Place, List[EnvironmentObject]])
  val env1: Environment = Environment(List(myRoom -> room2), Map(myRoom -> List(redBall)))

  // aka tile
  type Point2D = Vector2d[Int]
  type PlaceMap = Map[Point2D, Place]

  case class PassByStatistics(lastPassed: List[Boolean]) {
    def probabilityOfPassability: Double = {
      val ints = lastPassed.map(if(_) 1 else 0)
      ints.sum * 1.0/ints.length
    }
  }
  type PointMap = Map[Point2D, PassByStatistics]
  type WorldPointMap = Display[Boolean]
  type WorldCollisionShapes = List[CollisionShape[Double]]
  def convertWorldCollisionShapesToWorldPointMap(worldCollisionShapes: WorldCollisionShapes): WorldPointMap = {
    val points = worldCollisionShapes.flatMap(CollisionShape.toPoints(_))
    Display.showPoints(points, true, false)
  }
//  type WorldPointMap = Map[Point2D, Boolean] // Boolean - wall/free
//  sealed trait ObjectAtPosition
//  sealed trait Material extends ObjectAtPosition
//  case object Floor extends Material
//  case object Wall extends Material
//  case object Door extends Material
//
//  sealed trait Content extends ObjectAtPosition
  case class EnvironmentObject(name: String) //extends Content
//  case object UnidentifiedEnvironmentObject extends Content
//  case object MovableObject extends Material

  case class RobotEnvState(solidBodyState: SolidBodyState) {
    def handleCommand(materialParticleStateCommand: MaterialParticleStateCommand): RobotEnvState =
      RobotEnvState(solidBodyState.handleCommand(materialParticleStateCommand))
  }

  case class WorldState[S](robotEnvState: RobotEnvState, robotInternalState: S, worldCollisionShapes: WorldCollisionShapes, worldPointMapCache: WorldPointMap)

  case class RobotState(position: Point2D, rotation: Double, pointMap: PointMap,
                        placeMap: PlaceMap)
  // initially robot is at the dock station and has no map
  val initialState: RobotState = RobotState(Vector2d(0,0), 0.0, Map(), Map())
  // 1. Generate full map
  // 2. Walk robot around
  // 3. Render grey levels
  // 4. Scaffolding/dashboard - render.

  sealed trait ModellingEvent

  case class ScaffoldingTimePassed(wallTimeMs: Long) extends ModellingEvent

  trait ModelMechanics[S] {

    /** the returned Duration is the next event */
    def start(wallTimeMs: Long, internalState0: S): State[RandomStateValue, (WorldState[S], Duration)]
    /**
     *  Receives an event from scaffolding (like real timer, key press, mouse click).
     *  Despite that we try to perform deterministic modelling, we still need
     *  to generate white noise for signals. Hence, Random state.
     */
    def handleEvent(state: WorldState[S], e: ModellingEvent): State[RandomStateValue, (WorldState[S], Duration)]

//    /** Calculate next event. */
//    def nextTimerEventDelayMs(state: WorldState): Option[Long] = {
//      Some(40) // ~25 FPS
//    }

  }

  type Vector3dDouble = Vector3d[Double]
  sealed trait RobotSensorData
  /** There is an external timer that will trigger at certain intervals and report current time */
  case class TimePassed(sinceStartMs: Long) extends RobotSensorData
  /** This is an information that while moving forward we have hit an obstable.
   * Eventually we may have some details like robot side that was hit. */
  case class HitObstacle(sinceStartMs: Long) extends RobotSensorData
  /** The vectors are noisy and require filtering.
    */
  case class GyroscopeInfo(rotation: Vector3dDouble, acceleration: Vector3dDouble, magneticField: Vector3dDouble) extends RobotSensorData

  /** This is a magical sensor information that reports exact speed of the robot.
   * This should be superseded with just GyroscopeInfo.
   * */
  case class MagicalSpeedSensor(speed: Vector2d[Double]) extends RobotSensorData

  /** Information about surrounding area in the form of lidar samples. */
  case class LidarInfo(samples: LidarSamples) extends RobotSensorData

  sealed trait RobotCommand
  /**
   * Set speed of left and right motors. Range of speed is [0.0, 1.0].
   * There is some lowest speed like 0.5. If the speed is set below, motor doesn't start.
   */
  case class SetSpeed(left: Double, right: Double) extends RobotCommand
  /** Takes an object in front of the robot. */
//  case class Take() extends RobotCommand
  /** An interface to robot "brains". Mechanics will call this
   * strategy whenever something changes.  */
  type RobotStrategy[S] = (S, RobotSensorData) => (S, List[RobotCommand])
}
