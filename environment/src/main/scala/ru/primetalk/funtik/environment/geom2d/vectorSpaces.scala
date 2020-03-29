package ru.primetalk.funtik.environment.geom2d

import spire.algebra.{CModule, Field, VectorSpace}
import spire.sp
import spire.std.IntIsEuclideanRing

object IntField extends IntIsEuclideanRing with Field[Int] {
  override def div(x: Int, y: Int): Int = x / y
  override def euclideanFunction(a: Int): BigInt = BigInt(a).abs
  override def equot(a: Int, b: Int): Int        = spire.math.equot(a, b)
  override def emod(a: Int, b: Int): Int         = spire.math.emod(a, b)
}

/** An implementation of 2d VectorSpace for pairs of Ints and Doubles.
 * @tparam Axis is the type that is used to represent one of the coordinates.
 * @tparam Scalar is either Int or Double that we use to scale vectors.
 */
class VectorSpaceProduct2[@sp(Int, Double) Axis, @sp(Int, Double) Scalar](implicit val scalar: Field[Scalar], val cmodule: CModule[Axis, Scalar])
  extends VectorSpace[(Axis, Axis), Scalar]  {
  override def timesl(r: Scalar, v: (Axis, Axis)): (Axis, Axis) = (cmodule.timesl(r, v._1), cmodule.timesl(r, v._2))

  override def negate(v: (Axis, Axis)): (Axis, Axis) = (cmodule.negate(v._1), cmodule.negate(v._2))

  override def zero: (Axis, Axis) = (cmodule.zero, cmodule.zero)

  override def plus(a: (Axis, Axis), b: (Axis, Axis)): (Axis, Axis) = (cmodule.plus(a._1, b._1), cmodule.plus(a._2, b._2))
}

/** An implementation of 3d VectorSpace for 3-tuples of Ints and Doubles.
 * @tparam Axis is the type that is used to represent one of the coordinates.
 * @tparam Scalar is either Int or Double that we use to scale vectors.
 */
class VectorSpaceProduct3[@sp(Int, Double) Axis, @sp(Int, Double) Scalar](implicit val scalar: Field[Scalar], val cmodule: CModule[Axis, Scalar])
  extends VectorSpace[(Axis, Axis, Axis), Scalar] {
  override def timesl(r: Scalar, v: (Axis, Axis, Axis)): (Axis, Axis, Axis) = (cmodule.timesl(r, v._1), cmodule.timesl(r, v._2), cmodule.timesl(r, v._3))

  override def negate(v: (Axis, Axis, Axis)): (Axis, Axis, Axis) = (cmodule.negate(v._1), cmodule.negate(v._2), cmodule.negate(v._3))

  override def zero: (Axis, Axis, Axis) = (cmodule.zero, cmodule.zero, cmodule.zero)

  override def plus(a: (Axis, Axis, Axis), b: (Axis, Axis, Axis)): (Axis, Axis, Axis) = (cmodule.plus(a._1, b._1), cmodule.plus(a._2, b._2), cmodule.plus(a._3, b._3))
}

