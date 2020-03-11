package ru.primetalk.funtik.environment

import ru.primetalk.funtik.environment.geom2d.Geom2dUtils.Rectangle
import ru.primetalk.funtik.environment.genereator.{BSPTree, RoomNode, SpaceTree, SpaceTreeNode}


object BSPPrinter extends App{

  private def drawRandomRoom(size: Int = 120, minSideSize: Int = 6): Unit = {
    val rect = Rectangle(0 -> 0, size -> size)

    val partitionedSpace = new BSPTree(minSideSize = minSideSize).generate(rect)


    val array: Array[Array[String]] = Array.ofDim[String](rect.width, rect.height)

    def filledLine = Array.fill(rect.width)("* ")
    (0 until rect.height).foreach(array(_) = filledLine)


    def writeRect(rect: Rectangle, symbol: String): Unit = {
      for {
        x <- (rect.topLeft._1 + 1) until rect.bottomRight._1 //_1  + 1 ... ._2 --looks terrible
        y <- (rect.topLeft._2 + 1) until rect.bottomRight._2
      }{
        array(y)(x) = symbol
      }
    }

    def fillArray(tree: SpaceTree): Unit = {
      tree match {
        case s: SpaceTreeNode =>
          fillArray(s.left)
          fillArray(s.right)
          array(s.door.y)(s.door.x) = "0 "
        case r : RoomNode =>
          writeRect(r.rect, "  ")

      }
    }

    fillArray(partitionedSpace)

    val arraysAsAString = array.map(_.mkString).mkString("\n")
    println(arraysAsAString)
  }

  (1 to 1000).foreach{ i =>
    val roomMinSize = 5 + i % 3
    val areaSize = 50 + i % 50
    drawRandomRoom(areaSize, roomMinSize)
  }

}
