package ru.primetalk.funtik.environment

import ru.primetalk.funtik.environment.generator.utils.Random.RandomStateValue
import ru.primetalk.funtik.environment.geom2d.Geom2dUtils.vector2d

import scala.concurrent.duration.{ Duration, DurationInt, FiniteDuration }
import ru.primetalk.funtik.environment.generator.{ BSPTree, Tree }
import ru.primetalk.funtik.environment.geom2d.Geom2dUtils._
import ViewAll._
import cats.data.State

object MechanicsImpl extends ModelMechanics {

  private def toPoints: Tree[Rectangle] => List[(Int, Int)] =
    Tree.eliminate[Rectangle, List[Position]](_.edges)(_ reverse_::: _)

  def generateDisplay(rect: Rectangle): State[RandomStateValue, Display[Boolean]] =
    BSPTree()(rect).map(toPoints).map { points =>
      Display.showPoints(points, true, false)
    }

  val defaultDuration: FiniteDuration = 40.milliseconds

  /** the returned Duration is the next event */
  override def start: State[RandomStateValue, (ViewAll.WorldState, Duration)] = {
    val rect = Rectangle((-40, -30), (80, 60))
    generateDisplay(rect).map { display =>
      (
        WorldState(
          RobotEnvState(vector2d(0, 0), 0.0, 0.0),
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
    state: ViewAll.WorldState,
    e: ViewAll.ModellingEvent
  ): State[RandomStateValue, (ViewAll.WorldState, Duration)] = {
    State.pure(
      (
        state.copy(
          robotEnvState = state.robotEnvState.copy(
            position = state.robotEnvState.position + vector2d(0, 1)
          )
        ),
        defaultDuration
      )
    )
  }
}
