package ru.primetalk.funtik.environment

import ru.primetalk.funtik.environment.generator.utils.Random.RandomStateValue

import scala.concurrent.duration.{Duration, DurationInt, FiniteDuration}
import ru.primetalk.funtik.environment.generator.{BSPTree, Tree}
import ru.primetalk.funtik.environment.geom2d.Geom2dUtils._
import cats.data.State
import ru.primetalk.funtik.environment.geom2d.Vector2d
import ru.primetalk.funtik.environment.solid._
import ru.primetalk.funtik.environment.solid.SolidBodyModel._
import squants.time.Milliseconds

trait MechanicsImplT extends EnvironmentModel {

  class MechanicsImpl[S](val robotStrategy: RobotStrategy[S], val initialRobotState: S) extends ModelMechanics {

    private def toPoints: Tree[Rectangle] => List[Vector2d[Int]] =
      Tree.eliminate[Rectangle, List[Position]](_.edges)(_ reverse_::: _)

    def generateDisplay(rect: Rectangle): State[RandomStateValue, Display[Boolean]] =
      BSPTree()(rect).map(toPoints).map { points =>
        Display.showPoints(points, true, false)
      }

    val defaultDuration: FiniteDuration = 40.milliseconds

    /** the returned Duration is the next event */
    override def start(wallTimeMs: Long): State[RandomStateValue, (WorldState, Duration)] = {
      val rect = Rectangle(Vector2d(-40, -30), Vector2d(80, 60))
      generateDisplay(rect).map { display =>
        (
          WorldState(
            RobotEnvState(SolidBodyState(
              MaterialParticleState(
                position = Vector2d(10.0, 10.0),
                speed = Vector2d(0.0, 2.0),
                orthogonalAcceleration = -0.3,
                t = Milliseconds(wallTimeMs) / su.time
              ),
              theta = 0.0)),
            display
          ),
          defaultDuration
        )
      }
    }

    /**
     * Receives an event from scaffolding (like real timer, key press, mouse click).
     * Despite that we try to perform deterministic modelling, we still need
     * to generate white noise for signals. Hence, Random state.
     */
    override def handleEvent(
                              state: WorldState,
                              e: ModellingEvent
                            ): State[RandomStateValue, (WorldState, Duration)] = e match {
      case ScaffoldingTimePassed(wallTimeMs) =>
        State.pure(
          state.copy(
            robotEnvState = state.robotEnvState.copy(
              solidBodyState = state.robotEnvState.solidBodyState.integrate(Milliseconds(wallTimeMs) / su.time))
          ),
          defaultDuration
        )
    }
  }

}