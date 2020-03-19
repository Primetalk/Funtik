package ru.primetalk.funtik.environment

import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.ext.Color
import ru.primetalk.funtik.environment.geom2d.Geom2dUtils._

trait ViewWorldStateT extends ViewWorldMap with EnvironmentModel {
  implicit object ViewWorldState extends View[WorldState] {
    implicit val displayView: ViewDisplay[Boolean] =
      new ViewDisplay[Boolean](if(_) Color.Green else Color.apply("#70a0a0"))

    override def render(t: WorldState, ctx: CanvasRenderingContext2D): Unit = {
      t.worldPointMap.render(ctx)
      setTransform(t.worldPointMap, ctx)
      ctx.strokeStyle = Color.Red.toString()
      ctx.rotate(math.Pi - t.robotEnvState.rotation)
      ctx.lineWidth = 0.3
      ctx.moveTo(t.robotEnvState.position.x, t.robotEnvState.position.y - 2)
      ctx.lineTo(t.robotEnvState.position.x, t.robotEnvState.position.y + 2)
      ctx.moveTo(t.robotEnvState.position.x - 1, t.robotEnvState.position.y + 1)
      ctx.lineTo(t.robotEnvState.position.x, t.robotEnvState.position.y + 2)
      ctx.moveTo(t.robotEnvState.position.x + 1, t.robotEnvState.position.y + 1)
      ctx.lineTo(t.robotEnvState.position.x, t.robotEnvState.position.y + 2)
      ctx.fillStyle = Color.Red.toString()
      ctx.fillRect(-0.5,-0.5,1,1)
      ctx.stroke()
      clearTransform(ctx)
    }
  }
}
