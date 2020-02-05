package ru.primetalk.funtik.environment

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
  case class Point2D(x: Int, y: Int)
  type PlaceMap = Map[Point2D, Place]

  case class PassByStatistics(lastPassed: List[Boolean]) {
    def probabilityOfPassability: Double = {
      val ints = lastPassed.map(if(_) 1 else 0)
      ints.sum * 1.0/ints.length
    }
  }
  type PointMap = Map[Point2D, PassByStatistics]
  type WorldPointMap = Map[Point2D, Boolean] // Boolean - wall/free
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

  case class WorldState(position: Point2D, rotation: Double, pointMap: PointMap,
                        placeMap: PlaceMap)

  case class RobotState(position: Point2D, rotation: Double, pointMap: PointMap,
                        placeMap: PlaceMap)
  // initially robot is at the dock station and has no map
  val initialState: RobotState = RobotState(Point2D(0,0), 0.0, Map(), Map())
  // 1. Generate full map
  // 2. Walk robot around
  // 3. Render grey levels
  // 4. Scaffolding/dashboard - render.
}
