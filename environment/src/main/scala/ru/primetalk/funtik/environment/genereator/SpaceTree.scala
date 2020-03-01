package ru.primetalk.funtik.environment.genereator

import ru.primetalk.funtik.environment.Geom2dUtils.Rectangle

sealed trait SpaceTree{
  val rect: Rectangle
}

case class SpaceTreeNode(rect: Rectangle, left: SpaceTree, right: SpaceTree) extends SpaceTree

case class RoomNode(rect: Rectangle) extends SpaceTree