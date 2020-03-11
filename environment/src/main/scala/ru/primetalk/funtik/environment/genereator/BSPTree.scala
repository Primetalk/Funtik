package ru.primetalk.funtik.environment.genereator

import ru.primetalk.funtik.environment.geom2d.Geom2dUtils._
import ru.primetalk.funtik.environment.genereator.utils.{ManagedRandom, ScalaRandom}
import ru.primetalk.funtik.environment.geom2d.Axis2d
import Axis2d._

/**
  * @see https://en.wikipedia.org/wiki/Binary_space_partitioning
  */
class BSPTree(random: ManagedRandom = ScalaRandom, minSideSize: Int = 4, sidesMaxRatio: Double = 1.25f) {

  private val minSplittableSize = minSideSize * 2

  def generate(boundRect: Rectangle): SpaceTree = {
    def randomAxis = if(random.nextBoolean) Horizontal else Vertical
    val splitAxis = boundRect.getLongestAxis.map(transpose).getOrElse(randomAxis)
    val oppositeSize = calcOppositeSize(boundRect, splitAxis)
    if (oppositeSize > minSplittableSize) {
      val splitSize = random.between(minSideSize, oppositeSize - minSideSize)
      split(boundRect, splitAxis, splitSize)
    } else {
      RoomNode(boundRect)
    }
  }

  def calcOppositeSize(boundRect: Rectangle, splitAxis: Axis2d): Int = {
    val oppositeSide = transpose(splitAxis)
    val sideSize = boundRect.sideSize(oppositeSide)
    sideSize
  }

  def split(sourceRect: Rectangle, axis: Axis2d, splitSize: Int): SpaceTreeNode = {
    val (first, second) = sourceRect.split(axis, splitSize)
    val door = axis match {
      case Horizontal => Door(second.topLeft.x + 1, second.topLeft.y)
      case Vertical => Door(second.topLeft.x, second.topLeft.y + 1)
    }
    SpaceTreeNode(generate(first), generate(second), door)
  }
}

