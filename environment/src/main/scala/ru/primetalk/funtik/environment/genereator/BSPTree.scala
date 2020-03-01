package ru.primetalk.funtik.environment.genereator

import ru.primetalk.funtik.environment.Geom2dUtils._
import ru.primetalk.funtik.environment.genereator.utils.{ManagedRandom, ScalaRandom}

/**
  * @see https://en.wikipedia.org/wiki/Binary_space_partitioning
 */
class BSPTree(random: ManagedRandom = ScalaRandom, minSideSize: Int = 4, sidesMaxRatio: Double = 1.25f) {

  def generate(bound: Rectangle): SpaceTree = {
    val splitter = resolveSplitter(bound)
    splitter.split(bound) match {
      case None =>
        RoomNode(bound)
      case Some((f, s, d)) =>
        SpaceTreeNode(bound, generate(f), generate(s), d)
    }
  }

  private def resolveSplitter(rect: Rectangle): Splitter = {
//    val tooWide = rect.width2heightRatio >= sidesMaxRatio
//    val tooHigh = rect.height2widthRatio >= sidesMaxRatio
//    val splitHorizontally = (tooWide && !tooHigh) || random.nextBoolean

    val splitHorizontally = if(rect.height > rect.width) {
      true
    } else if(rect.width > rect.height){
      false
    } else {
      random.nextBoolean
    }

    val splittingSideSize = if(splitHorizontally) rect.height else rect.width
    val hasEnoughSize = splittingSideSize / 2 > minSideSize
    hasEnoughSize -> splitHorizontally match {
      case (false, _) => TooSmall
      case (true, true) => HorizontalSplitter(rect.height)
      case (true, false) => VerticalSplitter(rect.width)
    }

  }

  sealed abstract class Splitter(canContinue: Boolean){

    def split(rectangle: Rectangle): Option[(Rectangle, Rectangle, Door)]
  }

  case class HorizontalSplitter(height: Int) extends Splitter(true) {

    def split(sourceRect: Rectangle): Option[(Rectangle, Rectangle, Door)] = {
      val splitPosition  = random.between(minSideSize, height - minSideSize)

      val topSize = (sourceRect.width, splitPosition)
      val top = Rectangle(sourceRect.topLeft, topSize)

      val bottomStartY = sourceRect.startY + splitPosition - 1
      val bottomHeight = sourceRect.height - splitPosition + 1
      val bottom = Rectangle(
        sourceRect.startX -> bottomStartY,
        sourceRect.width -> bottomHeight
      )

      val door = Door(bottom.startX + 1, bottomStartY)
      Some((top, bottom, door))
    }
  }

  case class VerticalSplitter(width: Int) extends Splitter(true) {

    def split(rectangle: Rectangle): Option[(Rectangle, Rectangle, Door)] = {
      val splitPosition  = random.between(minSideSize, width - minSideSize)

      val leftSize = splitPosition -> rectangle.height
      val left = Rectangle(rectangle.topLeft, leftSize)

      val rightStartX = rectangle.startX + splitPosition - 1
      val rightWidth = rectangle.width - splitPosition + 1
      val right = Rectangle(
        rightStartX -> rectangle.startY,
        rightWidth -> rectangle.height
      )

      val door = Door(rightStartX, right.startY + 2)
      Some((left, right, door))
    }
  }

  case object TooSmall extends Splitter(false){

    def split(rectangle: Rectangle): Option[(Rectangle, Rectangle, Door)] = None
  }


}

