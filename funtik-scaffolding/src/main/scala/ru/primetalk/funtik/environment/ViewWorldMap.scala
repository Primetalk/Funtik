package ru.primetalk.funtik.environment
import org.scalajs.dom
import org.scalajs.dom.ext.Color
import ru.primetalk.funtik.environment.EnvironmentModel

trait View[T] {
  def render(t: T, ctx: dom.CanvasRenderingContext2D): Unit
}
object ViewWorldMap extends EnvironmentModel {
  implicit class ViewWorldMap[T](colorFun: T => Color) extends View[Display[T]] {
    def render(display: Display[T], ctx: dom.CanvasRenderingContext2D): Unit = {
//      ctx.scale(ctx.canvas.width * 1.0/t.size._1, ctx.canvas.height * 1.0/t.size._2)
//      ctx.offset(display.offset._1, display.offset._2)
      ctx.setTransform(ctx.canvas.width * 1.0/display.size._1, 0, 0, ctx.canvas.height * 1.0/display.size._2, -display.offset._1, -display.offset._2)
      for {
        p <- display.points
      } {
        val color = colorFun(display(p))
        ctx.fillStyle = color.toString()
        ctx.fillRect(p._1, p._2, 1.0, 1.0)
      }
      ctx.setTransform(1, 0, 0, 1, 0, 0)
    }

  }
}
