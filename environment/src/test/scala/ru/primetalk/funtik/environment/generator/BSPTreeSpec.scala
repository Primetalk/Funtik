package ru.primetalk.funtik.environment.generator

import org.specs2.Specification
import org.specs2.matcher.MatchResult
import ru.primetalk.funtik.environment.geom2d.Geom2dUtils.Rectangle
import ru.primetalk.funtik.environment.genereator._
import ru.primetalk.funtik.environment.geom2d.Geom2dUtils

class BSPTreeSpec extends Specification { def is = s2"""

  BSPTree should
    generate some rooms    $generate

  """
  def bspTree = new BSPTree

  def generate: MatchResult[Tree[Geom2dUtils.Rectangle]] = {
    val rect = Rectangle(0 -> 0, 100 -> 100)
    val result = bspTree.generate(rect)
    result must haveClass[Node[Rectangle]]
  }

}
