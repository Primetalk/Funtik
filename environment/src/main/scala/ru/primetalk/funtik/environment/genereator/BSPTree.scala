package ru.primetalk.funtik.environment.genereator

import ru.primetalk.funtik.environment.geom2d.Geom2dUtils._
import ru.primetalk.funtik.environment.genereator.utils.Random
import ru.primetalk.funtik.environment.geom2d.Axis2d
import Axis2d._
import cats.data.State
import ru.primetalk.funtik.environment.genereator.utils.Random.RandomState

/**
  * @see https://en.wikipedia.org/wiki/Binary_space_partitioning
  */
class BSPTree(minSideSize: Int = 4, sidesMaxRatio: Double = 1.25f) {

  private val minSplittableSize = minSideSize * 2

  def generate(boundRect: Rectangle): RandomState[Tree[Rectangle]] = {
    val splitAxis = boundRect.getLongestAxis.map(transposeState).getOrElse(Random.randomAxisState)
    for {
      axis <- splitAxis
      result <- trySplit(boundRect, axis)
    } yield result
  }

  def transposeState(axis: Axis2d): RandomState[Axis2d] = State.pure(transpose(axis))

  def trySplit(boundRect: Rectangle, splitAxis: Axis2d): RandomState[Tree[Rectangle]] = {
    val oppositeSize = calcOppositeSize(boundRect, splitAxis)
    if (oppositeSize > minSplittableSize) {
      splitRect(oppositeSize, boundRect, splitAxis)
    } else {
      State.pure(Leaf(boundRect))
    }
  }

  def calcOppositeSize(boundRect: Rectangle, splitAxis: Axis2d): Int = {
    val oppositeSide = transpose(splitAxis)
    val sideSize = boundRect.sideSize(oppositeSide)
    sideSize
  }

  def splitRect(oppositeSize: Int, boundRect: Rectangle, splitAxis: Axis2d): RandomState[Tree[Rectangle]] = {
    for{
      splitSize <- Random.randomBetween(minSideSize, oppositeSize - minSideSize)
      (firstRect, secondRect) = boundRect.split(splitAxis, splitSize)
      leftTree <- generate(firstRect)
      rightTree <- generate(secondRect)
    } yield {
      Node(leftTree, rightTree)
    }
  }
}
