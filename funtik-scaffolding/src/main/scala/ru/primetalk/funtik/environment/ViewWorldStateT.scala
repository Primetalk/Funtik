package ru.primetalk.funtik.environment

import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.ext.Color

trait ViewWorldStateT extends ViewWorldMap with EnvironmentModel {
  implicit object ViewWorldState extends View[WorldState] {
    implicit val displayView: ViewDisplay[Boolean] =
      new ViewDisplay[Boolean](if(_) Color.Green else Color.apply("#70a0a0"))

    override def render(worldState: WorldState, ctx: CanvasRenderingContext2D): Unit = {
      ctx.beginPath()
      worldState.worldPointMap.render(ctx)
      setTransform(worldState.worldPointMap, ctx)
      ctx.strokeStyle = Color.Red.toString()
      ctx.rotate(math.Pi - worldState.robotEnvState.solidBodyState.theta)
      ctx.lineWidth = 0.3
      val x = worldState.robotEnvState.solidBodyState.materialParticle.position.x
      val y = worldState.robotEnvState.solidBodyState.materialParticle.position.y
      ctx.moveTo(x, y - 2)
      ctx.lineTo(x, y + 2)
      ctx.moveTo(x - 1, y + 1)
      ctx.lineTo(x, y + 2)
      ctx.moveTo(x + 1, y + 1)
      ctx.lineTo(x, y + 2)
      ctx.fillStyle = Color.Red.toString()
      ctx.fillRect(x - 0.5, y - 0.5, x + 1, y + 1)
      ctx.stroke()
      clearTransform(ctx)
    }
  }
}
