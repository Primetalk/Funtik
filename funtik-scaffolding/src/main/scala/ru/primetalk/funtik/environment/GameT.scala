package ru.primetalk.funtik.environment

import org.scalajs.dom.CanvasRenderingContext2D
import ru.primetalk.funtik.core.RobotLoopT
import ru.primetalk.funtik.environment.geom2d.Vector2d

trait GameT extends ViewWorldStateT with RobotLoopT with MechanicsImplT with ControllerT with DefaultRobotDefinition {

  def startController(ctx: CanvasRenderingContext2D): Unit = {
    val t0 = System.currentTimeMillis()
    val controller = new Controller[InternalRobotState](ctx,
      new MechanicsImpl[InternalRobotState](handleSensorDataIgnoreAll, initialInternalRobotState),
      new ViewWorldState[InternalRobotState](),
      InternalRobotState(Vector2d(20, 20), Vector2d(0, 0), t0,
        Vector2d(0, 0), Display(Vector2d(0, 0), Vector2d(10, 10))()),
      t0)
    controller.start()
  }

}
