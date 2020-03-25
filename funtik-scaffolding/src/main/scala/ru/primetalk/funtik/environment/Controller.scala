package ru.primetalk.funtik.environment

import org.scalajs.dom
import ru.primetalk.funtik.environment.genereator.{BSPTree, Leaf, Tree, Node}
import ru.primetalk.funtik.environment.genereator.utils.Random
import ru.primetalk.funtik.environment.geom2d.Geom2dUtils._

import ViewAll._

// Mutable part of the program
class Controller
(
  val ctx: dom.CanvasRenderingContext2D,
  seed: Long
) {

  private var currentGameState: WorldState = {
    val display = generateDisplay(seed)
    WorldState(
      RobotEnvState(vector2d(0,0), 0.0, 0.0),
      display
    )
  }

  def generateDisplay(n: Long): Display[Boolean] = {
    val rect = Rectangle((-40, -30), (80, 60))
    val randoms = Random.stream(n)
    val roomTree = BSPTree()(rect).runA(randoms).value
    val points = toPoints(roomTree)
    Display.showPoints(points, true, false)
  }

  private def toPoints(tree: Tree[Rectangle]): List[Position] = tree match {
    case Node(l, r) => toPoints(l) reverse_::: toPoints(r)
    case Leaf(rect) => rect.edges
  }

  def redraw(): Unit = {
    currentGameState.render(ctx)
  }
}
