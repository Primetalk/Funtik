package ru.primetalk.funtik.environment.genereator

import ru.primetalk.funtik.environment.geom2d.Geom2dUtils._
import ru.primetalk.funtik.environment.genereator.utils.Random
import ru.primetalk.funtik.environment.geom2d.Axis2d
import Axis2d._


/**
  * @see https://en.wikipedia.org/wiki/Binary_space_partitioning
  */
class BSPTree(minSideSize: Int = 4, sidesMaxRatio: Double = 1.25f) {

  private val minSplittableSize = minSideSize * 2

  def generate(boundRect: Rectangle, randoms: LazyList[Int]): (Tree[Rectangle], LazyList[Int]) = {
    val (splitAxis, nextRandoms) = boundRect.getLongestAxis match {
      case Some(axis) => transpose(axis) -> randoms
      case None => Random.randomAxis(randoms.head) -> randoms.tail
    }
    val oppositeSize = calcOppositeSize(boundRect, splitAxis)
    if (oppositeSize > minSplittableSize) {
      val (splitSize, rand2) = Random.randomBetween(nextRandoms)(minSideSize, oppositeSize - minSideSize)
      val (firstRect, secondRect) = boundRect.split(splitAxis, splitSize)
      val (leftTree, rand3) = generate(firstRect, rand2)
      val (rightTree, rand4) = generate(secondRect, rand3)
      (Node(leftTree, rightTree), rand4)
    } else {
      (Leaf(boundRect), nextRandoms)
    }
  }

  def calcOppositeSize(boundRect: Rectangle, splitAxis: Axis2d): Int = {
    val oppositeSide = transpose(splitAxis)
    val sideSize = boundRect.sideSize(oppositeSide)
    sideSize
  }
}
