package ru.primetalk.funtik.environment.genereator

import ru.primetalk.funtik.environment.genereator.utils.Random._
import ru.primetalk.funtik.environment.geom2d.Axis2d
import ru.primetalk.funtik.environment.geom2d.Axis2d.{Horizontal, Vertical}

object EnvironmentRandom {

  def nextAxis: RandomState[Axis2d] =
    nextBoolean.map(if (_) Horizontal else Vertical)
}
