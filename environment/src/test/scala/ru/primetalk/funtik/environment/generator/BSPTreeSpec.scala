package ru.primetalk.funtik.environment.generator

import org.scalatest.{FlatSpec, Matchers}
import ru.primetalk.funtik.environment.Geom2dUtils.Rectangle
import ru.primetalk.funtik.environment.genereator.{BSPTree, SpaceTreeNode}

class BSPTreeSpec extends FlatSpec with Matchers{

  def bspTree = new BSPTree

  it should "should complete generate" in {
    val rect = Rectangle(0 -> 0, 100 -> 100)
    val result = bspTree.generate(rect)
    result shouldBe a [SpaceTreeNode]
  }

}
