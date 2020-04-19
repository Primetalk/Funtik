package ru.primetalk.funtik.environment

import org.scalajs.dom
import ru.primetalk.funtik.environment.generator.utils.Random
import ru.primetalk.funtik.environment.generator.utils.Random.RandomStateValue

import scala.concurrent.duration.Duration
import scala.scalajs.js.{Date, timers}

trait ControllerT extends ViewWorldStateT {

  // Mutable part of the program
  class Controller
  (
    val ctx: dom.CanvasRenderingContext2D,
    modelMechanics: ModelMechanics,
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
        start(realTimeMs).
        map(newStateAvailable).
        runS(randomStream).
        value

      def newStateAvailable: ((WorldState, Duration)) => Unit = {
        case (state, duration) =>
          state.render(ctx)
          timers.setTimeout(duration.toMillis) {
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