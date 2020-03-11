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

  def generate(boundRect: Rectangle): Tree[Rectangle] = {
    def randomAxis = if(random.nextBoolean) Horizontal else Vertical
    val splitAxis = boundRect.getLongestAxis.map(transpose).getOrElse(randomAxis)
    val oppositeSize = calcOppositeSize(boundRect, splitAxis)
    if (oppositeSize > minSplittableSize) {
      val splitSize = random.between(minSideSize, oppositeSize - minSideSize)
      val (first, second) = boundRect.split(splitAxis, splitSize)
      Node(generate(first), generate(second))
    } else {
      Leaf(boundRect)
    }
  }

  def calcOppositeSize(boundRect: Rectangle, splitAxis: Axis2d): Int = {
    val oppositeSide = transpose(splitAxis)
    val sideSize = boundRect.sideSize(oppositeSide)
    sideSize
  }

}

