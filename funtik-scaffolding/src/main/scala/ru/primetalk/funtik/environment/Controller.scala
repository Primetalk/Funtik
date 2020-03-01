package ru.primetalk.funtik.environment

import org.scalajs.dom

import scala.util.Random


// Mutable part of the program
class Controller
(
  val ctx: dom.CanvasRenderingContext2D,
  val gameView: View[Display[Boolean]]
) {
  // The following are the only two variables in the program
  private val rnd = new Random()
  private var currentGameState: Display[Boolean] = generateDisplay(rnd.nextInt())

  def generateDisplay(n: Int): Display[Boolean] = {
    Display.showPoints(Seq((1,1), (79,59)), true, false)
  }

  def redraw(): Unit = {
    gameView.render(currentGameState, ctx)
  }
}
