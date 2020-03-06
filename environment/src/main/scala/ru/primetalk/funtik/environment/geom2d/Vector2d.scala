package ru.primetalk.funtik.environment.geom2d

// @typeclass
trait Vector2d[V] {
  def getAxisValue(v: V, axis: Axis2d): Int
  def apply(p: (Int, Int)): V
  def unapply(v: V): (Int, Int) = (getAxisValue(v, Axis2d.Abscissa), getAxisValue(v, Axis2d.Ordinate))
}

trait Vector2dSyntax[V] {
  implicit class Vector2dOps(v: V)(implicit vector2d: Vector2d[V]){
    def x: Int = vector2d.getAxisValue(v, Axis2d.Abscissa)
    def y: Int = vector2d.getAxisValue(v, Axis2d.Ordinate)
    def _1: Int = vector2d.getAxisValue(v, Axis2d.Abscissa)
    def _2: Int = vector2d.getAxisValue(v, Axis2d.Ordinate)

    def +(vector: V): V =
      vector2d(_1 + vector._1, _2 + vector._2)
    def -(vector: V): V =
      vector2d(_1 - vector._1, _2 - vector._2)
    def *(k: Int): V = vector2d(x * k, y * k)

    def transpose: V =
      vector2d(v._2, v._1)

    /** Rotates as a multiplication of complex numbers. */
    def rotate(o: V): V =
      vector2d(v._1 * o._1 - v._2 * o._2, v._1 * o._2 + v._2 * o._1)

    def manhattanSize: Int = math.abs(v._1) + math.abs(v._2)

    def r: Double =
      math.sqrt(v._1*v._1 + v._2*v._2)

    /** Theta is an angle from X axis towards the given vector.
     * NB! The display has Y axis oriented down. So, in order to get normal
     * theta we inverse Y.*/
    def theta: Double = {
      math.atan2(-v._2, v._1)
    }
  }

  implicit class PairOps(p: (Int, Int))(implicit vector2d: Vector2d[V]){
    def toVector: V = vector2d(p)
  }

}

object Vector2dIntPair extends Vector2d[(Int, Int)] {
  override def getAxisValue(v: (Int, Int), axis: Axis2d): Int = axis match {
    case Axis2d.Abscissa => v._1
    case Axis2d.Ordinate => v._2
  }

  override def apply(p: (Int, Int)): (Int, Int) = p
}
