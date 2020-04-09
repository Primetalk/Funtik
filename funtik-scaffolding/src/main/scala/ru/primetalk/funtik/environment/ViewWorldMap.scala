package ru.primetalk.funtik.environment
import org.scalajs.dom
import org.scalajs.dom.ext.Color

trait View[T] {
  def render(t: T, ctx: dom.CanvasRenderingContext2D): Unit
}

trait ViewWorldMap extends EnvironmentModel {

  implicit class ViewOps[T: View](t: T) {
    def render(ctx: dom.CanvasRenderingContext2D): Unit =
      implicitly[View[T]].render(t, ctx)
  }

  def setTransform[T](display: Display[T], ctx: dom.CanvasRenderingContext2D): Unit = {
    val scaleX = ctx.canvas.width  * 1.0 / display.size._1
    val scaleY = ctx.canvas.height * 1.0 / display.size._2
    object scaleXYMatrix {
      val (a, b) = (scaleX, 0)
      val (c, d) = (0, -scaleY) // we want Y to go up
    }
    object offset {
      val (dx, dy) = (- (display.offset._1 - 0.5) * scaleX, - (display.offset._2 - 1.5) * scaleY) // -1.5 - empirical correction. Not sure why
    }
    val m = scaleXYMatrix
    // `setTransform` is expected to be equivalent to
    //      ctx.scale(scaleX, scaleY)
    //      ctx.offset(display.offset._1, display.offset._2)
    ctx.setTransform(
      m.a, m.b,
      m.c, m.d,
      offset.dx, offset.dy)

  }
  def clearTransform(ctx: dom.CanvasRenderingContext2D): Unit = {
    ctx.setTransform(1, 0, 0, 1, 0, 0)
  }
  class ViewDisplay[T](colorFun: T => Color) extends View[Display[T]] {
    def render(display: Display[T], ctx: dom.CanvasRenderingContext2D): Unit = {
      setTransform(display, ctx)
      for {
        p <- display.points
      } {
        val color = colorFun(display(p))
        ctx.fillStyle = color.toString()
        ctx.fillRect(p._1 - 0.5, p._2 - 0.5, 1.0, 1.0)
      }
      clearTransform(ctx)
    }

  }
}
