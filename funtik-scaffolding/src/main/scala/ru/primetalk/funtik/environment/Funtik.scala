package ru.primetalk.funtik.environment

import org.scalajs.dom
import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.raw.HTMLCanvasElement
import ru.primetalk.funtik.core.RobotLoopT

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("Funtik")
object Funtik {

  object Game extends GameT

  def main(args: Array[String]): Unit = {
    println(Game.currentRoom.name)
  }

  @JSExport("startFuntik")
  def startFuntik(canvasId: String): Unit = {
    val canvasElement = dom.document.getElementById(canvasId)
    val canvas = canvasElement.asInstanceOf[HTMLCanvasElement]
    val ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    Game.startController(ctx)
  }

}
