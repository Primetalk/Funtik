package ru.primetalk.funtik.environment.genereator.utils

import scala.util.Random

trait ManagedRandom {

  def nextInt(range: Int): Int

  def nextDouble: Double

  def nextBoolean: Boolean

  def between(minInclusive: Int, maxExclusive: Int): Int
}


object ScalaRandom extends ManagedRandom{
  override def nextInt(range: Int): Int = Random.nextInt(range)

  override def nextDouble: Double = Random.nextDouble()

  override def nextBoolean: Boolean = Random.nextBoolean()

  def between(minInclusive: Int, maxExclusive: Int): Int = Random.between(minInclusive, maxExclusive)
}