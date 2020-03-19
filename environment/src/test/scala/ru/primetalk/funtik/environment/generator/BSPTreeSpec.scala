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

  def generate: MatchResult[Tree[Geom2dUtils.Rectangle]] = {
    val rect = createSquare(100)
    val stream = Random.stream(10)
    val result = BSPTree()(rect).runA(stream).value
    result must haveClass[Node[Rectangle]]
  }

  private def createSquare(sideSize: Int) = {
    val topLeft = (0,0)
    val rectSize = (sideSize, sideSize)
    Rectangle(topLeft, rectSize)
  }

}
