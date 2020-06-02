package ru.primetalk.funtik.environment

import org.scalajs.dom
import org.scalajs.dom.CanvasRenderingContext2D
import ru.primetalk.funtik.environment.generator.utils.Random
import ru.primetalk.funtik.environment.generator.utils.Random.{RandomState, RandomStateValue}

import scala.concurrent.duration.Duration
import scala.scalajs.js.{Date, timers}

trait ControllerT extends ViewWorldStateT {

  // Mutable part of the program
  class Controller[S]
  (
    val ctx: dom.CanvasRenderingContext2D,
    modelMechanics: ModelMechanics[S],
    viewWorldState: ViewWorldState[S],
    initialState: S,
    seed: Long
  ) {

    def realTimeMs: Long = {
      val res = Date.now().toLong
      //    println(s"t: $res")
      res
    }

    def start(): Unit = {

      var randomStream: RandomStateValue = Random.stream(seed)
      randomStream = modelMechanics.
        start(realTimeMs, initialState).
        map(newStateAvailable).
        runS(randomStream).
        value

      def newStateAvailable: ((WorldState[S], Duration)) => Unit = {
        case (state, duration) =>
          viewWorldState.render(state, ctx)
          timers.setTimeout(duration.toMillis.toDouble) {
            val timePassed = ScaffoldingTimePassed(realTimeMs)
            randomStream = modelMechanics.handleEvent(state, timePassed).
              map(newStateAvailable).
              runS(randomStream).
              value
          }
      }

    }
  }

}