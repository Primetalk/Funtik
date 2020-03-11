package ru.primetalk.funtik.environment.generator

import org.specs2.Specification
import org.specs2.matcher.MatchResult
import ru.primetalk.funtik.environment.geom2d.Geom2dUtils.Rectangle
import ru.primetalk.funtik.environment.genereator.{BSPTree, SpaceTree, SpaceTreeNode}

class BSPTreeSpec extends Specification { def is = s2"""

  BSPTree should
    generate some rooms    $generate

  """
  def bspTree = new BSPTree

  def generate: MatchResult[SpaceTree] = {
    val rect = Rectangle(0 -> 0, 100 -> 100)
    val result = bspTree.generate(rect)
    result must haveClass[SpaceTreeNode]
  }

}
