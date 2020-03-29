package ru.primetalk.funtik.environment.geom2d

import algebra.instances.IntAlgebra
import algebra.ring.{CommutativeRing, Ring}
import spire.algebra.{CModule, Field, VectorSpace}
import spire.std.IntIsEuclideanRing
import spire.implicits._
import spire.math._
import spire.sp

// @typeclass
trait Vector2d[V] {
  def getAxisValue(v: V, axis: Axis2d): Int
  def apply(p: (Int, Int)): V
  def unapply(v: V): (Int, Int) = (getAxisValue(v, Axis2d.Abscissa), getAxisValue(v, Axis2d.Ordinate))
}
object Vector2d {
  implicit def vectorSpace[V](implicit vector2d: Vector2d[V]): VectorSpace[V, Int] =
    new Vector2dSpace[V]

}

/** Implementation of Spire VectorSpace for vector2d. */
class Vector2dSpace[Vector](implicit vector2d: Vector2d[Vector]) extends VectorSpace[Vector, Int] { //} with AdditiveAbGroup[Vector] {

  import Vector2dBasicSyntax._
  override def negate(v: Vector): Vector =
    vector2d(-v.x, -v.y)

  override def zero: Vector =
    vector2d((0, 0))

  override def plus(v1: Vector, v2: Vector): Vector =
    vector2d(v1.x + v2.x, v1.y + v2.y)

  override implicit def scalar: Field[Int] = new IntIsEuclideanRing with Field[Int] {
    override def euclideanFunction(a: Int): BigInt = BigInt(a).abs
    override def equot(a: Int, b: Int): Int        = spire.math.equot(a, b)
    override def emod(a: Int, b: Int): Int         = spire.math.emod(a, b)
    override def div(x: Int, y: Int): Int          = x / y
  }

  override def timesl(r: Int, v: Vector): Vector =
    vector2d(r * v.x, r * v.y)

}

object Vector2dBasicSyntax extends Vector2dBasicSyntax
trait Vector2dBasicSyntax {
  type Vector2dDouble = (Double, Double)

  implicit class Vector2dOps[Vector](vector: Vector)(implicit vector2d: Vector2d[Vector]) {
    def x: Int  = vector2d.getAxisValue(vector, Axis2d.Abscissa)
    def y: Int  = vector2d.getAxisValue(vector, Axis2d.Ordinate)
    def _1: Int = vector2d.getAxisValue(vector, Axis2d.Abscissa)
    def _2: Int = vector2d.getAxisValue(vector, Axis2d.Ordinate)

    //    def +(vector: Vector): Vector =
    //      vector2d(x + vector.x, y + vector.y)
    //    def -(vector: Vector): Vector =
    //      vector2d(x - vector.x, y - vector.y)
    //    def *(k: Int): Vector = vector2d(x * k, y * k)

    def transpose: Vector =
      vector2d(vector.y, vector.x)

    /** Rotates as a multiplication of complex numbers. */
    def rotate(other: Vector): Vector = {
      val x = vector.x * other.x - vector.y * other.y
      val y = vector.x * other.y + vector.y * other.x
      vector2d(x, y)
    }

    def manhattanSize: Int = math.abs(vector.x) + math.abs(vector.y)

    def r: Double =
      math.sqrt(x * x + y * y)

    /** Theta is an angle from X axis towards the given vector.
     * NB! The display has Y axis oriented down. So, in order to get normal
     * theta we inverse Y.*/
    def theta: Double = {
      math.atan2(-y, x)
    }

    def toDouble: Vector2dDouble = (x, y)

  }

  implicit class PairOps[Vector](p: (Int, Int))(implicit vector2d: Vector2d[Vector]) {
    def toVector: Vector = vector2d(p)
  }

}
object Vector2dSyntax extends Vector2dSyntax
trait Vector2dSyntax {
  type Vector2dDouble = (Double, Double)

  implicit class Vector2dDoubleOps(vector: Vector2dDouble) {
    def x: Double = vector._1
    def y: Double = vector._2
    def r: Double = math.sqrt(x * x + y * y)
    def normalized: Vector2dDouble = {
      val rr = r
      (x / rr, y / rr)
    }

    def +(other: Vector2dDouble): Vector2dDouble =
      (
        vector._1 + other._1,
        vector._2 + other._2
      )
    def -(other: Vector2dDouble): Vector2dDouble =
      (
        vector._1 - other._1,
        vector._2 - other._2
      )

    def *(k: Double): Vector2dDouble = (x * k, y * k)

    def /(k: Double): Vector2dDouble = (x / k, y / k)
  }
}

object Vector2dIntPair extends Vector2d[(Int, Int)] {
  override def getAxisValue(v: (Int, Int), axis: Axis2d): Int = axis match {
    case Axis2d.Abscissa => v._1
    case Axis2d.Ordinate => v._2
  }

  override def apply(p: (Int, Int)): (Int, Int) = p
}
