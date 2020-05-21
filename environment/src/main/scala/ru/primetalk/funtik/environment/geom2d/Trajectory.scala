package ru.primetalk.funtik.environment.geom2d

import spire.implicits._
import Vector._
import ru.primetalk.funtik.environment.geom2d.lines2d.{EquationLine, TwoPointsLine}
import DoublePrecision.DoubleCompareOps
//import spire.algebra.CModule

sealed trait Trajectory {
  /** Returns position and velocity at the given moment. */
  def integrate(t: Double): (Vector2d[Double], Vector2d[Double])
  /** Returns parameter value for the point.
   * If point is not on the trajectory, the result is undefined. */
  def pointToParameter(p: Vector2d[Double]): Double

  def intersect(other: Trajectory): List[(Double, Double)] =
    Trajectory.intersectTrajectory(this, other)

  def t0: Double
}

object Trajectory {
  /** Linear trajectory is given by the following equation:
   * {{{
   * r = r0 + v * (t - t0)
   * }}}
   */
  case class Linear(r0: Vector2d[Double], velocity: Vector2d[Double], t0: Double) extends Trajectory {
    def toEquationLine: EquationLine[Double] =
      toTwoPointsLine.toEquationLine
    def toTwoPointsLine: TwoPointsLine =
      TwoPointsLine(r0, r0 + velocity)
    /** A function that calculates parameter for a point on a line.
     * NB! it doesn't check if the point indeed on the line. If not - the result is undefined.
     *
     * r = r0 + v * (t - t0)
     * t = t0 + (r - r0) / v
     * */
    def pointToParameter(r: Vector2d[Double]): Double = {
      val dr = r - r0
      t0 + (if(math.abs(velocity.x) > math.abs(velocity.y)) {
        dr.x / velocity.x
      } else {
        dr.y / velocity.y
      })
    }

    override def integrate(t: Double): (Vector2d[Double], Vector2d[Double]) =
      (r0 + (velocity :* (t - t0)), velocity)
  }

  case class Circular(center: Vector2d[Double], radius: Double, t0: Double, phase0: Double, omega: Double) extends Trajectory {
    override def integrate(t: Double): (Vector2d[Double], Vector2d[Double]) = {
      val phase = (t - t0) * omega + phase0
      (
        center + Vector2dPolar(radius, phase).toVector2d,
        Vector2dPolar(radius * omega, phase + math.Pi / 2).toVector2d
        )
    }

    def updateTrajectory(t: Double): Circular = {
      val (p, _) = integrate(t)
      val phase = (p - center).toPolar.theta
      this.copy(t0 = t, phase0 = phase)
    }
    /** Returns parameter value for the point.
     * If point is not on the trajectory, the result is undefined. */
    override def pointToParameter(p: Vector2d[Double]): Double = {
      val phase = (p - center).toPolar.theta
      val positiveDeltaPhase =
        (4 * math.Pi + (
          if (omega > 0) {
            phase - phase0
          } else {
            phase0 - phase
          }
        )) % (2 * math.Pi)
      t0 + math.abs(positiveDeltaPhase / omega)
    }
  }

  val epsilon = 1e-10

  /** Finds intersection of two lines. If there is an intersection,
   * it returns `t1` and `t2` - parameter values for each line where they intersect.
   * It solves the following equation
   * r1 = r10 + v1 * (t1 - t10) == r2 = r20 + v2 * t2
   * v1 * (t1 - t10) - v2 * (t2 - t20) == - (r10 - r20)
   * t = (t1 - t10, t2 - t20)  -- vector 2x1
   * v = (v1, -v2) -- matrix 2x2
   * v * (t - t0) == r20 - r10
   * vInv = v ** -1
   * vInv * v * (t - t0) == vInv * (r20-r10)
   * I        * (t - t0) == vInv * (r20-r10)
   * t = vInv * (r20 - r10) + t0
   */
  def intersection(l1: Linear, l2: Linear)(implicit doublePrecision: DoublePrecision): List[(Double, Double)] = {
    val v1 = l1.velocity
    val v2 = l2.velocity
    val r10 = l1.r0
    val r20 = l2.r0
    val v = Matrix2d[Double](v1.x, -v2.x, v1.y, -v2.y)
    val (vInv, determinant) = v.inverse
    Option.when(determinant !~ 0.0)  {
      val t = vInv * (r20 - r10)
      val (t1, t2) = t.toTuple
      (t1 + l1.t0, t2 + l2.t0)
    }.toList
  }

  /**
   * @see Weisstein, Eric W. "Circle-Line Intersection." From MathWorld--A Wolfram Web Resource. https://mathworld.wolfram.com/Circle-LineIntersection.html
   */
  def intersection(c1: Circular, l2: Linear)(implicit doublePrecision: DoublePrecision): List[(Double, Double)] = {
    val center = c1.center
    val r = c1.radius
    val TwoPointsLine(p10, p20) = l2.toTwoPointsLine
    val TwoPointsLine(Vector2d(x1, y1), Vector2d(x2, y2)) = lines2d.TwoPointsLine(p10 - center, p20 - center)
    val dx = x2 - x1
    val dy = y2 - y1
    val `dr^2` = dx * dx + dy * dy
    val D = x1 * y2 - x2 * y1
    val discriminant = r * r * `dr^2` - D * D
    Option.when(discriminant >=~ 0) {
      val sqrtDiscriminant = math.sqrt(math.abs(discriminant))
      val resultX1 = (D * dy + math.signum(dy) * dx * sqrtDiscriminant)/`dr^2`
      val resultX2 = (D * dy - math.signum(dy) * dx * sqrtDiscriminant)/`dr^2`
      val resultY1 = (-D * dx + math.abs(dy) * sqrtDiscriminant)/`dr^2`
      val resultY2 = (-D * dx - math.abs(dy) * sqrtDiscriminant)/`dr^2`
      val result1 = Vector2d[Double](resultX1, resultY1)
      val result2 = Vector2d[Double](resultX2, resultY2)
      val results = List(result1, result2)
      results.
        map(_ + center). // back to normal coordinates
        map(result => (c1.pointToParameter(result), l2.pointToParameter(result)))
    }.toList.flatten
  }

  /**
   * @see  Weisstein, Eric W. "Circle-Circle Intersection." From MathWorld--A Wolfram Web Resource. https://mathworld.wolfram.com/Circle-CircleIntersection.html
   */
  def intersection(c1: Circular, c2: Circular)(implicit doublePrecision: DoublePrecision): List[(Double, Double)] = {
    val axisX = c2.center - c1.center
    val distanceBetweenCenters = axisX.length
    val d = distanceBetweenCenters
    val R = c1.radius
    val r = c2.radius
    if (d >~ r + R)
      Nil
    else {
      val x = (d * d - r * r + r * R) / 2 / d
      val `y^2` = R * R - x * x
      require(`y^2` >=~ 0, "Unexpected condition that y^2 < 0")
      val y1 = math.sqrt(math.abs(`y^2`))
      val y2 = -y1
      val v1 = Vector2d(x, y1)
      val v2 = Vector2d(x, y2)
      val vectors = List(v1, v2)
      val theta = axisX.toPolar.theta
      val points = vectors.map(v => v.rotate(theta) + c1.center)
      points.map(p =>
        (c1.pointToParameter(p), c2.pointToParameter(p))
      )
    }
  }

  def intersectTrajectory(t1: Trajectory, t2: Trajectory)(implicit doublePrecision: DoublePrecision): List[(Double, Double)] = (t1, t2) match {
    case (l1: Linear, l2: Linear) => intersection(l1, l2)
    case (c1: Circular, l2: Linear) => intersection(c1, l2)
    case (c1: Circular, c2: Circular) => intersection(c1, c2)
    case (l1: Linear, c2: Circular) => intersection(c2, l1).map(_.swap)
  }

  def detectNearestCollision(trajectory: Trajectory, shape: CollisionShape[Double]): Option[Double] = (trajectory, shape) match {
    case (l1: Linear, CollisionShape.Line(p1, p2)) =>
      val l2 = lines2d.TwoPointsLine(p1, p2).toParametricLineDouble(0)
      intersection(l1, l2).flatMap{ case (t1, _) =>
        Option.when(t1 > trajectory.t0)(t1)
      }.headOption
    case (l1: Linear, CollisionShape.LineSegment(p1, p2)) =>
      val l2 = lines2d.TwoPointsLine(p1, p2).toParametricLineDouble(0)
      intersection(l1, l2).flatMap{ case (t1, t2) =>
        Option.when(t1 > trajectory.t0 &&
          t2 >= 0 && t2 <= 1 // we intersect between line segment ends
        )(t1)
      }.headOption
    case (_, p: CollisionShape.Polygon[Double]) =>
      p.toLineSegments.flatMap(detectNearestCollision(trajectory, _)).minOption
    case (l1: Linear, CollisionShape.Circle(center, radius)) =>
      val c2 = Circular(center, radius, t0 = 0, phase0 = 0, omega = 1)
      intersection(c2, l1).
        map(_._2).
        filter(_ >= l1.t0).
        minOption
    case (c1: Circular, CollisionShape.Line(p1, p2)) =>
      val l2 = lines2d.
        TwoPointsLine(p1, p2).
        toParametricLineDouble(0.0)
      intersection(c1, l2).
        map(_._1).
        filter(_ >= c1.t0).
        minOption
    case (c1: Circular, CollisionShape.LineSegment(p1, p2)) =>
      val l2 = lines2d.
        TwoPointsLine(p1, p2).
        toParametricLineDouble(0.0)

      val list = intersection(c1, l2)
      list.foreach(t => require(t._1 >= c1.t0))
      val filtered = list.
        filter(t =>
          t._2 >= 0 && t._2 <= 1 &&
            t._1 > c1.t0)
      println(s"t0=${c1.t0}; c1.r=${c1.radius}; omega=${c1.omega} list: $list; filtered=$filtered; $c1")
      filtered.
        map(_._1).
        minOption
    case (c1: Circular, c2: CollisionShape.Circle[Double]) =>
      val c2t = Circular(c2.center, c2.radius, trajectory.t0, 0.0, 1.0)
      c1.intersect(c2t).map(_._1).filter(_ > c1.t0).minOption
  }

}
