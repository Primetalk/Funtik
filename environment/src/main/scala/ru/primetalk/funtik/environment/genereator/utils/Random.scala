package ru.primetalk.funtik.environment.genereator.utils

import cats.data.State

object Random {

  type RandomStateValue = LazyList[Long]

  type RandomState[T] = State[RandomStateValue, T]

  def stream(seed: Long): LazyList[Long] = {
    val random = new scala.util.Random(seed)
    def randomStream0 : LazyList[Long] = random.nextLong() #:: randomStream0
    randomStream0
  }

  def between(minInclusive: Int, maxExclusive: Int): RandomState[Int] = {
    val difference = maxExclusive.toLong - minInclusive
    nextLong.map(long => (Math.floorMod(long, difference) + minInclusive).toInt)
  }

  def betweenOpt(minInclusive: Int, maxExclusive: Int): RandomState[Option[Int]] = {
    val difference = maxExclusive.toLong - minInclusive
    if(difference <= 0) {
      State.pure(None)
    } else {
      nextLong.map(long => Some((Math.floorMod(long, difference) + minInclusive).toInt))
    }
  }

  def nextLong: RandomState[Long] = State(rands => (rands.tail, rands.head))

  def nextDouble(min: Double = 0.0, max: Double = 1.0): RandomState[Double] = {
    val delta = max - min
    val deltaLong = Long.MaxValue.toDouble - Long.MinValue
    nextLong.map(
      l => (l.toDouble - Long.MinValue) * delta / deltaLong + min
    )
  }

  def nextAbsInt: RandomState[Int] = nextLong.map(l => Math.abs(l.toInt))

  def nextInt: RandomState[Int] = nextLong.map(_.toInt)

  def nextBoolean: RandomState[Boolean] = nextLong.map(_ % 2 == 0)

  def nextBooleanProb(p: Double): RandomState[Boolean] = nextDouble().map{ _ < p}

  def nextProb(p: Double): RandomState[Option[Unit]] = nextBooleanProb(p).map{ Option.when(_)(()) }
}
