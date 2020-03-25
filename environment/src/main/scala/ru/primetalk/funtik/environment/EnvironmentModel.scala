package ru.primetalk.funtik.environment

import cats.data.State
import ru.primetalk.funtik.environment.genereator.utils.Random.{RandomState, RandomStateValue}
import ru.primetalk.funtik.environment.geom2d.Geom2dUtils._

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
//  case class Point2D(x: Int, y: Int)
  type Point2D = Vector
  type PlaceMap = Map[Point2D, Place]

  case class PassByStatistics(lastPassed: List[Boolean]) {
    def probabilityOfPassability: Double = {
      val ints = lastPassed.map(if(_) 1 else 0)
      ints.sum * 1.0/ints.length
    }
  }
  type PointMap = Map[Point2D, PassByStatistics]
  type WorldPointMap = Display[Boolean]
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

  case class RobotEnvState(position: Point2D, speed: Double, rotation: Double)

  case class WorldState(robotEnvState: RobotEnvState, worldPointMap: WorldPointMap)

  case class RobotState(position: Point2D, rotation: Double, pointMap: PointMap,
                        placeMap: PlaceMap)
  // initially robot is at the dock station and has no map
  val initialState: RobotState = RobotState(vector2d(0,0), 0.0, Map(), Map())
  // 1. Generate full map
  // 2. Walk robot around
  // 3. Render grey levels
  // 4. Scaffolding/dashboard - render.

  sealed trait ModellingEvent

  case class TimePassed(wallTimeMs: Long) extends ModellingEvent

  trait ModelMechanics {

    /** the returned Duration is the next event */
    def start: State[RandomStateValue, (WorldState, Duration)]
    /**
     *  Receives an event from scaffolding (like real timer, key press, mouse click).
     *  Despite that we try to perform deterministic modelling, we still need
     *  to generate white noise for signals. Hence, Random state.
     */
    def handleEvent(state: WorldState, e: ModellingEvent): State[RandomStateValue, (WorldState, Duration)]

//    /** Calculate next event. */
//    def nextTimerEventDelayMs(state: WorldState): Option[Long] = {
//      Some(40) // ~25 FPS
//    }

  }
}
