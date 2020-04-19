package ru.primetalk.funtik.environment.geom2d

import spire.algebra.Field
import spire.std.IntIsEuclideanRing

object IntField extends IntIsEuclideanRing with Field[Int] {
  override def div(x: Int, y: Int): Int = x / y
  override def euclideanFunction(a: Int): BigInt = BigInt(a).abs
  override def equot(a: Int, b: Int): Int        = spire.math.equot(a, b)
  override def emod(a: Int, b: Int): Int         = spire.math.emod(a, b)
}
