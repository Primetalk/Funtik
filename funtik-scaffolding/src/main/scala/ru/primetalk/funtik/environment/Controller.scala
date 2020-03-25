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
  private var currentGameState: Display[Boolean] = generateDisplay(System.currentTimeMillis())//rnd.nextInt())

  def generateDisplay(n: Long): Display[Boolean] = {
    val rect = Rectangle((-40, -30), (79, 59))
    val randoms = Random.stream(n)
    val roomTree = BSPTree(leafProbability = 0.1)(rect).runA(randoms).value
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
