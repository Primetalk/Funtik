package ru.primetalk.funtik.environment.genereator.utils

import cats.data.State
import scala.util.Random

object Random {

  type RandomState[T] = State[LazyList[Long], T]

  def stream(seed: Long): LazyList[Long] = {
    val random = new Random(seed)
    def randomStream0 : LazyList[Long] = random.nextLong() #:: randomStream0
    randomStream0
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
