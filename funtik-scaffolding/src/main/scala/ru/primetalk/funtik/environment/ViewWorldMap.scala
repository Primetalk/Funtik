package ru.primetalk.funtik.environment
import org.scalajs.dom
import org.scalajs.dom.ext.Color

trait View[T] {
  def render(t: T, ctx: dom.CanvasRenderingContext2D): Unit
}

object ViewWorldMap extends EnvironmentModel {
  class ViewWorldMapImpl[T](colorFun: T => Color) extends View[Display[T]] {
    def render(display: Display[T], ctx: dom.CanvasRenderingContext2D): Unit = {
//      ctx.scale(ctx.canvas.width * 1.0/t.size._1, ctx.canvas.height * 1.0/t.size._2)
//      ctx.offset(display.offset._1, display.offset._2)
      object transformationMatrix {
        val (a, b) = (ctx.canvas.width * 1.0/(display.size._1 + 1), 0)
        val (c, d) = (0, ctx.canvas.height * 1.0/(display.size._2 + 1))
      }
      object offset {
        val (dx, dy) = (-display.offset._1, -display.offset._2)
      }
      val m = transformationMatrix
      ctx.setTransform(
        m.a, m.b,
        m.c, m.d,
        offset.dx, offset.dy)
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
