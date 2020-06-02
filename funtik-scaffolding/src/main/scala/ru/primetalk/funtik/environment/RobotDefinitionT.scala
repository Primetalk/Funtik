package ru.primetalk.funtik.environment

import ru.primetalk.funtik.environment.solid.RobotGeometry
import ru.primetalk.funtik.environment.solid.SolidBodyModel.su
import squants.space.Centimeters
trait RobotDefinitionT {
  val robot: RobotGeometry
}

trait DefaultRobotDefinition extends RobotDefinitionT {
  override val robot: RobotGeometry = RobotGeometry(
    width = Centimeters(20)/su.length,
    wheelRadius = Centimeters(5.7)/su.length,
  )

}