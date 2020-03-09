package ru.primetalk.funtik.environment.genereator

import ru.primetalk.funtik.environment.Geom2dUtils._
import ru.primetalk.funtik.environment.genereator.utils.{ManagedRandom, ScalaRandom}

/**
  * @see https://en.wikipedia.org/wiki/Binary_space_partitioning
  */
class BSPTree(random: ManagedRandom = ScalaRandom, minSideSize: Int = 4, sidesMaxRatio: Double = 1.25f) {

  def generate(boundRect: Rectangle): SpaceTree = {
    def randomAxis = if(random.nextBoolean) Horizontal else Vertical
    val splitAxis = boundRect.getLongestAxis.map(transpose).getOrElse(randomAxis)
    val sideSize = boundRect.sideSize(splitAxis)
    val restSideSize = sideSize - minSideSize
    if (restSideSize > minSideSize) {
      val splitSize = random.between(minSideSize, restSideSize)
      split(boundRect, splitAxis, splitSize)
    } else {
      RoomNode(boundRect)
    }
  }

  def split(sourceRect: Rectangle, axis: Axis, splitSize: Int): SpaceTreeNode = {
    val (first, second) = sourceRect.split(axis, splitSize)
    val door = axis match {
      case Horizontal => Door(second.startX + 1, second.startY)
      case Vertical => Door(second.startX, second.startY + 1)
    }
    SpaceTreeNode(generate(first), generate(second), door)
  }
}

