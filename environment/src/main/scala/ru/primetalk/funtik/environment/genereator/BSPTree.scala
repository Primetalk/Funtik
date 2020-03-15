package ru.primetalk.funtik.environment.genereator

import ru.primetalk.funtik.environment.geom2d.Geom2dUtils._
import ru.primetalk.funtik.environment.genereator.utils.Random
import ru.primetalk.funtik.environment.geom2d.Axis2d
import Axis2d._
import cats.data.{OptionT, State}
import ru.primetalk.funtik.environment.genereator.utils.Random.RandomState

/**
  * @see https://en.wikipedia.org/wiki/Binary_space_partitioning
  */
class BSPTree(minSideSize: Int = 4) {

  private val minSplittableSize = minSideSize * 2

  def generate(boundRect: Rectangle): RandomState[Tree[Rectangle]] = {
    val longestAxis = boundRect.getLongestAxis
    val splitAxis = OptionT
      .fromOption[RandomState](longestAxis)
      .getOrElseF(EnvironmentRandom.nextAxis)
    splitAxis
      .flatMap{axis =>
        val sideSize = boundRect.sideSize(axis)
        if(sideSize > minSplittableSize){
          splitRect(sideSize, boundRect, transpose(axis))
        } else {
          State.pure(Leaf(boundRect))
        }
      }
  }

  def splitRect(oppositeSize: Int,
                boundRect: Rectangle,
                splitAxis: Axis2d): RandomState[Tree[Rectangle]] = {
    for {
      splitSize <- Random.between(minSideSize, oppositeSize - minSideSize)
      (firstRect, secondRect) = boundRect.split(splitAxis, splitSize)
      leftTree <- generate(firstRect)
      rightTree <- generate(secondRect)
    } yield {
      Node(leftTree, rightTree)
    }
  }
}
