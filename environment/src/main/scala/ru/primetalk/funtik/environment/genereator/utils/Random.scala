package ru.primetalk.funtik.environment.genereator.utils
import ru.primetalk.funtik.environment.geom2d.Axis2d
import ru.primetalk.funtik.environment.geom2d.Axis2d.{Horizontal, Vertical}

import scala.util.Random

object Random {

  def stream(seed: Long): LazyList[Int] = {
    def randomStream0(random: Random): LazyList[Int] = {
      val i = random.nextInt(Int.MaxValue)
      i #:: randomStream0(random)
    }
    randomStream0(new Random(seed))
  }

  def randomAxis(nextInt: Int): Axis2d = if(nextInt % 2 == 0) Horizontal else Vertical

  def randomBetween(randoms: LazyList[Int])(minInclusive: Int, maxExclusive: Int): (Int, LazyList[Int]) = {
    val random = randoms.head
    val difference = maxExclusive - minInclusive
    val result = random % difference + minInclusive
    (result, randoms.tail)
  }

}
