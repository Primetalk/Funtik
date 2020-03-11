package ru.primetalk.funtik.environment

import ru.primetalk.funtik.environment.geom2d.Geom2dUtils._
import ru.primetalk.funtik.environment.genereator._


object BSPPrinter extends App{

  private def drawRandomRoom(size: Int = 120, minSideSize: Int = 6): Unit = {
    val rect = Rectangle(0 -> 0, size -> size)

    val partitionedSpace = new BSPTree(minSideSize = minSideSize).generate(rect)


    val array: Array[Array[String]] = Array.ofDim[String](rect.width, rect.height)

    def filledLine = Array.fill(rect.width)("* ")
    (0 until rect.height).foreach(array(_) = filledLine)


    def writeRect(rect: Rectangle, symbol: String): Unit = {
      for {
        x <- (rect.topLeft.x + 1) until rect.bottomRight.x
        y <- (rect.topLeft.y + 1) until rect.bottomRight.y
      }{
        array(y)(x) = symbol
      }
    }

    def fillArray(tree: Tree[Rectangle]): Unit = {
      tree match {
        case Node(left, right) =>
          fillArray(left)
          fillArray(right)
        case Leaf(rectangle) =>
          writeRect(rectangle, "  ")
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
