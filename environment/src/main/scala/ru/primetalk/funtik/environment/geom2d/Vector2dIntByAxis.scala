package ru.primetalk.funtik.environment.geom2d

// @typeclass
trait Vector2dIntByAxis[V] {
  def getAxisValue(v: V, axis: Axis2d): Int
  def apply(p: (Int, Int)): V
  def unapply(v: V): (Int, Int) = (getAxisValue(v, Axis2d.Abscissa), getAxisValue(v, Axis2d.Ordinate))
}

object Vector2dBasicSyntax extends Vector2dBasicSyntax
trait Vector2dBasicSyntax {

  implicit class PairOps(p: (Int, Int)){
    def toVector: Vector2d[Int] = Vector2d(p._1, p._2)
  }

}
