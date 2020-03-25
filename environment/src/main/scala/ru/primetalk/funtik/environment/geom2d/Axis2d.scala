package ru.primetalk.funtik.environment.geom2d

sealed trait Axis2d

object Axis2d {
  case object Abscissa extends Axis2d
  case object Ordinate extends Axis2d

  val Horizontal: Abscissa.type = Abscissa
  val Vertical: Ordinate.type = Ordinate

  def transpose(axis2d: Axis2d): Axis2d = axis2d match {
    case Abscissa => Ordinate
    case Ordinate => Abscissa
  }
}
