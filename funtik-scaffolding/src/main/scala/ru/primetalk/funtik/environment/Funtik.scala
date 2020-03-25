package ru.primetalk.funtik.environment

import org.scalajs.dom
import org.scalajs.dom.raw.HTMLCanvasElement
import org.scalajs.dom.ext.Color
import ru.primetalk.funtik.environment.ViewAll.ViewWorldMapImpl

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("Funtik")
object Funtik {
  def main(args: Array[String]): Unit = {
    println(ViewAll.currentRoom.name)
  }

  @JSExport("startFuntik")
  def startFuntik(canvasId: String): Unit = {
    val canvasElement = dom.document.getElementById(canvasId)
    val canvas = canvasElement.asInstanceOf[HTMLCanvasElement]
    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    val controller = new Controller(ctx, new ViewWorldMapImpl[Boolean](f => if(f) Color.Green else Color.apply("#70a0a0")))
    controller.redraw()
  }

}
