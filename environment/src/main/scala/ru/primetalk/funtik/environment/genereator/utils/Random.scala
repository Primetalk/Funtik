package ru.primetalk.funtik.environment.genereator.utils
import cats.data.State
import ru.primetalk.funtik.environment.geom2d.Axis2d
import ru.primetalk.funtik.environment.geom2d.Axis2d.{Horizontal, Vertical}

import scala.util.Random

object Random {

  type RandomState[T] = State[LazyList[Int], T]

  def stream(seed: Long): LazyList[Int] = {
    def randomStream0(random: Random): LazyList[Int] = {
      val i = random.nextInt(Int.MaxValue)
      i #:: randomStream0(random)
    }
    randomStream0(new Random(seed))
  }

  def randomAxis(nextInt: Long): Axis2d = if(nextInt % 2 == 0) Horizontal else Vertical

  def randomAxisState: RandomState[Axis2d] = State{rands =>
    val axis = randomAxis(rands.head)
    rands.tail -> axis
  }

  def randomBetween(minInclusive: Int, maxExclusive: Int): RandomState[Int] = State{randoms =>
    val random = randoms.head
    val difference = maxExclusive - minInclusive
    val result = random % difference + minInclusive
    (randoms.tail, result)
  }

}
