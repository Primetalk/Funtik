package ru.primetalk.funtik.environment.genereator

import ru.primetalk.funtik.environment.Geom2dUtils.Rectangle

sealed trait SpaceTree{
  val rect: Rectangle
}

case class SpaceTreeNode(rect: Rectangle, left: SpaceTree, right: SpaceTree, door: Door) extends SpaceTree

case class RoomNode(rect: Rectangle) extends SpaceTree

case class Door(x: Int, y: Int)