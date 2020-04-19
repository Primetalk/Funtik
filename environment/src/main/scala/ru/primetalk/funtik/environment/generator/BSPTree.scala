package ru.primetalk.funtik.environment.generator

import ru.primetalk.funtik.environment.geom2d.Geom2dUtils._
import ru.primetalk.funtik.environment.generator.utils.Random
import ru.primetalk.funtik.environment.geom2d.Axis2d
import Axis2d._
import cats.data.OptionT
import ru.primetalk.funtik.environment.generator.utils.Random.RandomState

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
    def calcSplitPositionOpt(axis: Axis2d): RandomState[Option[Int]] = {
      val sideSize = boundRect.sideSize(axis)
      Random.betweenOpt(minSideSize, sideSize - minSideSize)
    }

    def splitAt(boundRect: Rectangle, longerAxis: Axis2d)(position: Int): RandomState[Tree[Rectangle]] = {
      val shorterAxis = transpose(longerAxis)
      val innerRectangles     = boundRect.splitAsList(shorterAxis, position)
      val innerRandomTrees: List[RandomState[Tree[Rectangle]]] = innerRectangles.map(apply(minSideSize, leafProbability))
      val randomTreeList: RandomState[List[Tree[Rectangle]]] = Random.listRandomToRandomList(innerRandomTrees)
      randomTreeList.map(listOfTrees => Tree.constructNode(listOfTrees: _*))
    }

    val longerAxisToCut = OptionT
      .fromOption[RandomState](boundRect.getLongestAxis)
      .getOrElseF(EnvironmentRandom.nextAxis)

    longerAxisToCut.flatMap { longerAxis =>

      val shouldSplit = Random.nextProb(1.0 - leafProbability)

      OptionT(shouldSplit)
        .flatMapF {_ => calcSplitPositionOpt(longerAxis) }
        .semiflatMap(splitAt(boundRect, longerAxis))
        .fold(Leaf(boundRect): Tree[Rectangle])(identity)
    }
  }

}
