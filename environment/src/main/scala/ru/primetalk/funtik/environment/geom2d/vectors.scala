package ru.primetalk.funtik.environment.geom2d

import spire.algebra.{CModule, CRing, Field, VectorSpace}
import spire.sp

sealed trait Vector[Axis, Size <: Int]

case class Vector2d[@sp(Int, Double) Axis](x: Axis, y: Axis) extends Vector[Axis, 2] {
  def _1: Axis = x
  def _2: Axis = y
  def transpose: Vector2d[Axis] =
    Vector2d(y, x)
  def toTuple: (Axis, Axis) = (x, y)
}

case class Vector3d[@sp(Int, Double) Axis](x: Axis, y: Axis, z: Axis) extends Vector[Axis, 3] {
  def _1: Axis = x
  def _2: Axis = y
  def _3: Axis = z
}

case class Vector2dPolar(r: Double, theta: Double) extends Vector[Double, 2] {
  def toVector2d: Vector2d[Double] =
    Vector2d(r * math.cos(theta), r * math.sin(theta))
}

object Vector {
  implicit class Vector2dIntOps(v: Vector2d[Int]) {
    def toDouble: Vector2d[Double] = Vector2d(v.x, v.y)
  }

  implicit class Vector2dOps[Axis](v: Vector2d[Axis])(implicit num: Numeric[Axis]) {
    def manhattanSize: Axis =
      num.plus(num.abs(v.x), num.abs(v.y))
  }

  implicit class Vector2dDoubleOps(v: Vector2d[Double]) {
    def length: Double = math.sqrt(v.x * v.x + v.y * v.y)
    def normalized: Vector2d[Double] = {
      val rr = length
      Vector2d(v.x / rr, v.y / rr)
    }
    def /(k: Double): Vector2d[Double] = Vector2d(v.x / k, v.y / k)

    def toPolar: Vector2dPolar =
      Vector2dPolar(length, math.atan2(v.y, v.x))

    def rotate(alpha: Double): Vector2d[Double] = {
      val p = toPolar
      p.copy(theta = p.theta + alpha).toVector2d
    }
  }

  implicit object vector2dIntByAxis extends Vector2dIntByAxis[Vector2d[Int]] {
    override def getAxisValue(v: Vector2d[Int], axis: Axis2d): Int = axis match {
      case Axis2d.Abscissa => v.x
      case Axis2d.Ordinate => v.y
    }
    override def apply(p: (Int, Int)): Vector2d[Int] = Vector2d(p._1, p._2)
  }

  def vectorSpaceForVector2d[Axis, Scalar](implicit cmodule: CModule[Axis, Scalar],field: Field[Scalar]): VectorSpace[Vector2d[Axis], Scalar] = new VectorSpace[Vector2d[Axis], Scalar] {
    override implicit def scalar: Field[Scalar] = field
    override def timesl(r: Scalar, v: Vector2d[Axis]): Vector2d[Axis] = Vector2d(cmodule.timesl(r, v.x), cmodule.timesl(r, v.y))
    override def negate(v: Vector2d[Axis]): Vector2d[Axis] = Vector2d(cmodule.negate(v.x), cmodule.negate(v.y))
    override def zero: Vector2d[Axis] = Vector2d(cmodule.zero, cmodule.zero)
    override def plus(a: Vector2d[Axis], b: Vector2d[Axis]): Vector2d[Axis] = Vector2d(cmodule.plus(a.x, b.x), cmodule.plus(a.y, b.y))
  }

  def vectorSpaceForVector3d[Axis, Scalar](implicit cmodule: CModule[Axis, Scalar],field: Field[Scalar]): VectorSpace[Vector3d[Axis], Scalar] = new VectorSpace[Vector3d[Axis], Scalar] {
    override implicit def scalar: Field[Scalar] = field
    override def timesl(r: Scalar, v: Vector3d[Axis]): Vector3d[Axis] = Vector3d(cmodule.timesl(r, v.x), cmodule.timesl(r, v.y), cmodule.timesl(r, v.z))
    override def negate(v: Vector3d[Axis]): Vector3d[Axis] = Vector3d(cmodule.negate(v.x), cmodule.negate(v.y), cmodule.negate(v.z))
    override def zero: Vector3d[Axis] = Vector3d(cmodule.zero, cmodule.zero, cmodule.zero)
    override def plus(a: Vector3d[Axis], b: Vector3d[Axis]): Vector3d[Axis] = Vector3d(cmodule.plus(a.x, b.x), cmodule.plus(a.y, b.y), cmodule.plus(a.z, b.z))
  }

  import spire.std._

//  implicit val intField: Field[Int] = IntField
  import spire.std.double.DoubleAlgebra

  implicit val intCModule: CModule[Int, Int] = new IntIsEuclideanRing with CModule[Int, Int] {
    override implicit def scalar: CRing[Int] = int.IntAlgebra

    override def timesl(r: Int, v: Int): Int = r * v
  }

  implicit val doubleCModule: CModule[Double, Double] = new DoubleIsField with CModule[Double, Double]{
    override implicit def scalar: CRing[Double] = DoubleAlgebra

    override def timesl(r: Double, v: Double): Double = r * v
  }
  implicit val vectorSpaceForVector2dInt: VectorSpace[Vector2d[Int], Int] = vectorSpaceForVector2d[Int, Int](intCModule, IntField)
  implicit val vectorSpaceForVector2dDouble: VectorSpace[Vector2d[Double], Double] = vectorSpaceForVector2d[Double, Double]
  implicit val vectorSpaceForVector3dDouble: VectorSpace[Vector3d[Double], Double] = vectorSpaceForVector3d[Double, Double]
}
