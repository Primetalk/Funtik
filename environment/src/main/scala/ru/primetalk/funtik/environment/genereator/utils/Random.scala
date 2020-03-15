package ru.primetalk.funtik.environment.genereator.utils
import cats.data.State
import ru.primetalk.funtik.environment.geom2d.Axis2d
import ru.primetalk.funtik.environment.geom2d.Axis2d.{Horizontal, Vertical}

import scala.util.Random

object Random {

  type RandomState[T] = State[LazyList[Long], T]

  def stream(seed: Long): LazyList[Long] = {
    def randomStream0(random: Random): LazyList[Long] = {
      val i = random.nextLong()
      i #:: randomStream0(random)
    }
    randomStream0(new Random(seed))
  }

  def axis(nextInt: Long): Axis2d = if(nextInt % 2 == 0) Horizontal else Vertical

  def axisState: RandomState[Axis2d] = State{rands =>
    val axis = Random.axis(rands.head)
    rands.tail -> axis
  }

  def between(minInclusive: Int, maxExclusive: Int): RandomState[Int] = State{randoms =>
    val random = Math.abs(randoms.head.toInt)
    val difference = maxExclusive - minInclusive
    val result = random % difference + minInclusive
    (randoms.tail, result)
  }

}
