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
    initialRect: Rectangle,
    minSideSize: Int = 4,
    leafProbability: Double = 0.5
  ): RandomState[Tree[Rectangle]] = {
    generate(initialRect)(BSPTreeParams(minSideSize, leafProbability = leafProbability))
  }

  private case class BSPTreeParams(minSideSize: Int = 4, leafProbability: Double = 0.1) {
    val minSplittableSize: Int = minSideSize * 2
  }

  private def generate(
    boundRect: Rectangle
  )(implicit ctx: BSPTreeParams = BSPTreeParams()
  ): RandomState[Tree[Rectangle]] = {
    val longestAxis = boundRect.getLongestAxis
    val splitAxis = OptionT
      .fromOption[RandomState](longestAxis)
      .getOrElseF(EnvironmentRandom.nextAxis)
    splitAxis.flatMap { axis =>
      val shouldSplit = Random.nextProb(1.0 - ctx.leafProbability)

      def calcSplitPositionOpt(unit: Unit): RandomState[Option[Int]] = {
        val sideSize = boundRect.sideSize(axis)
        Random.betweenOpt(ctx.minSideSize, sideSize - ctx.minSideSize)
      }

      def split(splitPosition: Int): RandomState[Tree[Rectangle]] = {
        val splitAxis = transpose(axis)
        val rects     = boundRect.splitL(splitAxis, splitPosition)
        val rectTrees = rects.map(generate)
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
