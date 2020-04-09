package ru.primetalk.funtik.environment

import org.scalajs.dom
import ru.primetalk.funtik.environment.genereator.{BSPTree, Leaf, Node, Tree}
import ru.primetalk.funtik.environment.genereator.utils.Random
import ru.primetalk.funtik.environment.geom2d.Geom2dUtils._
import ViewAll._
import cats.data.State
import ru.primetalk.funtik.environment.genereator.utils.Random.RandomStateValue

import scala.concurrent.duration.Duration
import scala.scalajs.js.{Date, timers}

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
