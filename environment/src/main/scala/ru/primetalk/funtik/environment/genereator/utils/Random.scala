package ru.primetalk.funtik.environment.genereator.utils

import cats.data.State
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

  def between(minInclusive: Int, maxExclusive: Int): RandomState[Int] = {
    val difference = maxExclusive.toLong - minInclusive
    nextLong.map(long => (Math.floorMod(long, difference) + minInclusive).toInt)
  }

  def nextLong: RandomState[Long] = State(rands => (rands.tail, rands.head))

  def nextAbsInt: RandomState[Int] = nextLong.map(l => Math.abs(l.toInt))

  def nextInt: RandomState[Int] = nextLong.map(_.toInt)

  def nextBoolean: RandomState[Boolean] = nextLong.map(_ % 2 == 0)
}
