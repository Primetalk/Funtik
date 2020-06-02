package ru.primetalk.funtik.environment

import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.ext.Color

trait ViewWorldStateT extends ViewWorldMap with EnvironmentModel {
  class ViewWorldState[S] extends View[WorldState[S]] {
    implicit val displayView: ViewDisplay[Boolean] =
      new ViewDisplay[Boolean](if(_) Color.Green else Color.apply("#70a0a0"))

    override def render(worldState: WorldState[S], ctx: CanvasRenderingContext2D): Unit = {
      ctx.beginPath()
      worldState.worldPointMap.render(ctx)
      setTransform(worldState.worldPointMap, ctx)
      ctx.strokeStyle = Color.Red.toString()
      ctx.lineWidth = 0.3
      ctx.fillStyle = Color.Red.toString()
      val x = worldState.robotEnvState.solidBodyState.materialParticle.position.x
      val y = worldState.robotEnvState.solidBodyState.materialParticle.position.y
      ctx.translate(x, y) // set coordinates origin to the center of the robot
      ctx.rotate(worldState.robotEnvState.solidBodyState.theta) // rotate the shape of the robot according to the current angle
      ctx.moveTo(-2, 0)
      ctx.lineTo(2, 0)
      ctx.moveTo(1, -1)
      ctx.lineTo(2, 0)
      ctx.moveTo(1, 1)
      ctx.lineTo(2, 0)
      ctx.fillRect(- 0.5, - 0.5, 1, 1)
      ctx.stroke()
      clearTransform(ctx)
    }
  }
}
