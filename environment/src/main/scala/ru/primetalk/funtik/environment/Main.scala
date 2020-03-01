package ru.primetalk.funtik.environment

import ru.primetalk.funtik.environment.Geom2dUtils.Rectangle
import ru.primetalk.funtik.environment.genereator.{BSPTree, RoomNode, SpaceTree, SpaceTreeNode}


object Main extends App{

  val size =  30
  val rect = Rectangle(0 -> 0, size -> size)

  val partitionedSpace = new BSPTree(minSideSize = 7).generate(rect)


  val array: Array[Array[String]] = Array.ofDim[String](rect.width, rect.height)

  def arr = Array.fill(rect.width)("* ")
  for{
    j <- 0 until rect.height
  } {
    array(j) = arr
  }

  fillArray(partitionedSpace)
//  writeRect(rect, ". ")


  val string = array.map(_.mkString).mkString("\n")
  println(partitionedSpace)



  println(string)


  def fillArray(tree: SpaceTree): Unit = {
    tree match {
      case s: SpaceTreeNode =>
        fillArray(s.left)
        fillArray(s.right)
      case r : RoomNode =>
        writeRect(r.rect, "  ")

    }
  }

  def writeRect(rect: Rectangle, symbol: String): Unit = {
    for {
      x <- (rect.startX + 1) until rect.endX
      y <- (rect.startY + 1) until rect.endY
    }{
      array(y)(x) = symbol
    }

  }


}
