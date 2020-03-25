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
    val rect = Geom2dUtils.square(100)
    val stream = Random.stream(13)
    val result = BSPTree(leafProbability = 0.01)(rect).runA(stream).value
    result must haveClass[Node[Rectangle]]
  }

}
