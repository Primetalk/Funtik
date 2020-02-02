package ru.primetalk.funtik.core

import ru.primetalk.funtik.environment.EnvironmentModel
// Task should be able to represent the intent "bring me the red ball from my room".
trait Tasks extends  EnvironmentModel {
  sealed trait ObjectSpecification
  case class LocationInfo(place: Place) extends ObjectSpecification
  case class ObjectName(name: String) extends ObjectSpecification

  sealed trait Task

  /**
   * Bring some object that satisfies the list of attributes.
   */
  case class BringAnObject(attributes: List[ObjectSpecification]) extends Task

  val task: Task = BringAnObject(List(ObjectName("red ball"), LocationInfo(myRoom)))

}
