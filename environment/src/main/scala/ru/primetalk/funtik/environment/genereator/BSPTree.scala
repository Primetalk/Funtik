package ru.primetalk.funtik.environment.genereator

import ru.primetalk.funtik.environment.geom2d.Geom2dUtils._
import ru.primetalk.funtik.environment.genereator.utils.Random
import ru.primetalk.funtik.environment.geom2d.Axis2d
import Axis2d._
import cats.implicits._
import cats.data.OptionT
import ru.primetalk.funtik.environment.genereator.utils.Random.RandomState

/**
 * @see https://en.wikipedia.org/wiki/Binary_space_partitioning
 */
object BSPTree {
  def apply(
    minSideSize: Int = 4,
    leafProbability: Double = 0.1
  )(
    boundRect: Rectangle
  ): RandomState[Tree[Rectangle]] = {
    val longestAxis = boundRect.getLongestAxis
    val splitAxis = OptionT
      .fromOption[RandomState](longestAxis)
      .getOrElseF(EnvironmentRandom.nextAxis)
    splitAxis.flatMap { axis =>
      val shouldSplit = Random.nextProb(1.0 - leafProbability)

      def calcSplitPositionOpt(unit: Unit): RandomState[Option[Int]] = {
        val sideSize = boundRect.sideSize(axis)
        Random.betweenOpt(minSideSize, sideSize - minSideSize)
      }

      def split(splitPosition: Int): RandomState[Tree[Rectangle]] = {
        val splitAxis = transpose(axis)
        val rects     = boundRect.splitAsList(splitAxis, splitPosition)
        val rectTrees = rects.map(apply(minSideSize, leafProbability))
        val rRects = rectTrees.foldM(List[Tree[Rectangle]]()) {
          case (lst, rTree) =>
            rTree.map(tree => tree :: lst)
        }
        rRects.map(lst => Tree.apply(lst: _*))
      }

      OptionT(shouldSplit).flatMapF { calcSplitPositionOpt }
        .semiflatMap(split)
        .fold[Tree[Rectangle]](Leaf(boundRect))(identity[Tree[Rectangle]])
    }
  }

}
