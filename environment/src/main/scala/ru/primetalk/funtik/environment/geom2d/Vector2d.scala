package ru.primetalk.funtik.environment.geom2d

// @typeclass
trait Vector2d[V] {
  def getAxisValue(v: V, axis: Axis2d): Int
  def apply(p: (Int, Int)): V
  def unapply(v: V): (Int, Int) = (getAxisValue(v, Axis2d.Abscissa), getAxisValue(v, Axis2d.Ordinate))
}

trait Vector2dSyntax[Vector] {
  type Vector2dDouble = (Double, Double)

  implicit class Vector2dOps(vector: Vector)(implicit vector2d: Vector2d[Vector]){
    def x: Int = vector2d.getAxisValue(vector, Axis2d.Abscissa)
    def y: Int = vector2d.getAxisValue(vector, Axis2d.Ordinate)
    def _1: Int = vector2d.getAxisValue(vector, Axis2d.Abscissa)
    def _2: Int = vector2d.getAxisValue(vector, Axis2d.Ordinate)

    def +(vector: Vector): Vector =
      vector2d(x + vector.x, y + vector.y)
    def -(vector: Vector): Vector =
      vector2d(x - vector.x, y - vector.y)
    def *(k: Int): Vector = vector2d(x * k, y * k)

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

  implicit class PairOps(p: (Int, Int))(implicit vector2d: Vector2d[Vector]){
    def toVector: Vector = vector2d(p)
  }

  implicit class Vector2dDoubleOps(vector: Vector2dDouble){
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
