package ru.primetalk.funtik.environment

import org.scalajs.dom
import ru.primetalk.funtik.environment.genereator.{BSPTree, Leaf, Tree, Node}
import ru.primetalk.funtik.environment.genereator.utils.Random
import ru.primetalk.funtik.environment.geom2d.Geom2dUtils._

import scala.util.{Random => ScalaRandom}


// Mutable part of the program
class Controller
(
  val ctx: dom.CanvasRenderingContext2D,
  val gameView: View[Display[Boolean]]
) {
  private val rnd = new ScalaRandom()
  private var currentGameState: Display[Boolean] = generateDisplay(rnd.nextInt())

  def generateDisplay(n: Int): Display[Boolean] = {
    val rect = Rectangle(0 -> 0, 79 -> 59)
    val randoms = Random.stream(System.currentTimeMillis())
    val roomTree = new BSPTree(10).generate(rect).runA(randoms).value
    val points = toPoints(roomTree)
    Display.showPoints(points, true, false)
  }

  private def toPoints(tree: Tree[Rectangle]): List[Position] = tree match {
    case Node(l, r) => toPoints(l) reverse_::: toPoints(r)
    case Leaf(rect) => rect.edges
  }


  def redraw(): Unit = {
    gameView.render(currentGameState, ctx)
  }
}
