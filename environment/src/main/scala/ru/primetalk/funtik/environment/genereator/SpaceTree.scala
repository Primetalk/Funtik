package ru.primetalk.funtik.environment.genereator

import ru.primetalk.funtik.environment.Geom2dUtils.Rectangle

sealed trait SpaceTree

case class SpaceTreeNode(
    left: SpaceTree,
    right: SpaceTree,
    door: Door //TODO remove door from this class
) extends SpaceTree

case class RoomNode(rect: Rectangle) extends SpaceTree

case class Door(x: Int, y: Int)