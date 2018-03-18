package animation

import java.lang.Math._

object Animation extends App {


  def nextPoint(c: Vector, w: Double, angle: Int): Vector = {
    val radAngle = toRadians(angle)
    val x = c.x + w * cos(radAngle)
    val y = c.y + w * sin(radAngle)
    Vector(x, y)
  }

  def angleForSide(side: Int) = side % 3 match{
    case 0 => 0
    case 1 => 120
    case _ => -120
  }

  def superThree(start: Vector, sideLen: Int): List[Vector] = {
    val short = sideLen
    val long = sideLen + sideLen / 6
    var pp = start
    val units = for {i <- 0 until 5} yield {
      val angle = angleForSide(i % 3)
      val len = if (i % 2 == 0) short else long
      val np = nextPoint(pp, len, angle)
      pp = np
      np
    }
    start :: units.toList
  }

  def cframe(offI: Int, p: List[Vector]): List[Vector] = {
    val List(p0, p1) = p.take(2)
    val List(p25, p3) = p.takeRight(2)
    val off  = offI / 100.0
    //val List(p0, p1, p2, p25, p3)  = p
    val np0  = p0 + ((p1 - p0) / p1.length(p0)) * off * p1.length(p0)
    val np3 = p25 + ((p3 - p25) / p3.length(p25)) * p3.length(p25) * off
    List(np0, p1) ::: p.drop(2).dropRight(2) ::: List(p25, np3)
  }

  def superThreeFrame(start: Vector, sideLen: Int, frame: Int): List[Vector] ={
    val xies = superThree(start, sideLen)
    val frameRelative = frame % xies.size
    val parts = 6
    (xies ::: xies).slice(frameRelative, frameRelative + parts)
  }

  def circleRatio(sideLen: Double): Double =
    sqrt(pow(sideLen, 2) - pow(sideLen / 2, 2)) * 2/3

  def startingPointCenter(center: Vector, sideLen: Double): Vector = {
    val supportVectorLen = circleRatio(sideLen)
    nextPoint(center, supportVectorLen, 360-150)
  }

  //def render()

}
