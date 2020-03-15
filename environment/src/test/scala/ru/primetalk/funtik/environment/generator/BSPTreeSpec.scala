package ru.primetalk.funtik.environment.generator

import org.specs2.Specification
import org.specs2.matcher.MatchResult
import ru.primetalk.funtik.environment.geom2d.Geom2dUtils.Rectangle
import ru.primetalk.funtik.environment.genereator._
import ru.primetalk.funtik.environment.genereator.utils.Random
import ru.primetalk.funtik.environment.geom2d.Geom2dUtils

class BSPTreeSpec extends Specification { def is = s2"""

  BSPTree should
    generate some rooms    $generate

  """
  def bspTree = new BSPTree

  def generate: MatchResult[Tree[Geom2dUtils.Rectangle]] = {
    val rect = Rectangle(0 -> 0, 100 -> 100)
    val stream = Random.stream(10)
    val (result, _) = bspTree.generate(rect, stream)
    result must haveClass[Node[Rectangle]]
  }

}
