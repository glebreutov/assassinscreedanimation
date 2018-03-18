package animation

import java.lang.Math._

case class Vector(x: Double, y: Double){

  def +(xy: Vector): Vector = {
    Vector(x + xy.x, y + xy.y)
  }

  def -(xy: Vector): Vector = {
    Vector(x - xy.x, y- xy.y)
  }

  def / (i: Double): Vector = {
    Vector(x/i, y/i)
  }

  def *(i: Double): Vector = {
    Vector(x*i, y*i)
  }

  def length(p1: Vector): Double = {
    sqrt(pow(x - p1.x, 2) + pow(y - p1.y, 2))
  }

  def abs: Vector = {
    Vector(x.abs, y.abs)
  }

  def slope: Double = x / y

  def rotate(origin: Vector, angle: Double): Vector = {
    val angleRad = toRadians(angle)
    //x_rotated = ((x - x_origin) * cos(angle)) - ((y_origin - y) * sin(angle)) + x_origin
    //y_rotated = ((y_origin - y) * cos(angle)) - ((x - x_origin) * sin(angle)) + y_origin
    val x2 = cos(angleRad) * (x - origin.x) - sin(angleRad) * (y - origin.y) + origin.x
    val y2 = sin(angleRad) * (x - origin.x) + cos(angleRad) * (y - origin.y) + origin.y
    Vector(x2, y2)
  }

  def pointOnLine(to: Vector, len: Double): Vector = {
    val step = (to - this) / length(to)
    this + step * len
  }
}

object Vector{
  def normVector(v1: Vector, v2: Vector): Vector = {
    val vd = v2 - v1
    val penpd= Vector(vd.y, -vd.x)
    penpd / vd.length(penpd)
  }
}
