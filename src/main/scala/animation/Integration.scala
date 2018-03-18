package animation

import java.lang.Math.toRadians

import org.scalajs.dom
import org.scalajs.dom.ext.KeyCode

import scala.scalajs.js
import org.scalajs.dom.html.{Button, Canvas}
import org.scalajs.dom.raw.HTMLImageElement
import Animation._
case class Position(x: Int, y: Int)


object Integration extends js.JSApp {

  def main(): Unit = {
    initGame
  }


  def initGame(): Unit = {

    val canvas = dom.document.createElement("canvas").asInstanceOf[Canvas]
    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

    val size = 0.95 * Math.min(dom.window.innerHeight, dom.window.innerWidth)
    canvas.height = size.toInt
    canvas.width = size.toInt
    dom.document.body.appendChild(canvas)

    val origin = Vector(canvas.height / 2, canvas.height / 2)

    val sideLen = (canvas.height * 0.6).intValue
    val outerTriangleLen = sideLen + sideLen / 3
    val startOuter = startingPointCenter(origin, outerTriangleLen)
    val startStart = startOuter + Vector((outerTriangleLen - sideLen) / 2, 0)

    println(startStart)
    def reset(): Unit ={
      ctx.fillStyle = "black"
      ctx.fillRect(0, 0, canvas.width, canvas.height)
      ctx.strokeStyle = "grey"
      ctx.lineWidth = 1
      ctx.beginPath()
      ctx.arc(origin.x, origin.y, circleRatio(sideLen + (outerTriangleLen - sideLen) / 3.0), 0.0, 360.0)
      ctx.arc(origin.x, origin.y, circleRatio(outerTriangleLen), 0.0, 360.0)
      val p1 = nextPoint(startOuter, outerTriangleLen, 0 )
      val p2=  nextPoint(p1, outerTriangleLen, 120)
      val p3 = nextPoint(p2, outerTriangleLen, -120)
      ctx.moveTo(p1.x, p1.y)
      ctx.lineTo(p2.x, p2.y)
      ctx.lineTo(p3.x, p3.y)
      ctx.lineTo(p1.x, p1.y)
//      ctx.arc(origin.x, origin.y, 350 * 2 / 3, 0.0, 360.0)
//      ctx.moveTo(origin.x, origin.y)
//      ctx.lineTo(startStart.x, startStart.y)
      ctx.stroke()
    }



    def drawMe2(state: SceneState): Unit = {
      reset()
      ctx.strokeStyle = "orange"
      ctx.fillStyle = "rgba(253,164, 40, 1)"
      ctx.lineWidth = 1
      ctx.strokeText(state.stage.toString, 10, 10)
      ctx.strokeText(state.frame.toString, 30, 10)
      ctx.strokeText(state.angle.toString, 60, 10)
      ctx.lineWidth = 1


      val dots = cframe(state.frame, superThreeFrame(startStart, sideLen, state.stage))
        .map(_.rotate(origin, state.angle))

      //val dots = superThree(startStart, sideLen)
      def normDot(from: Vector, to: Vector, width: Double, lineBoldness: Double): Vector = {
        val step = (to - from) / from.length(to)

        val offset1 = from + step * width

        offset1 - Vector.normVector(offset1, to) * lineBoldness
      }
      def lineToRect(from: Vector, to: Vector, maxH: Double = 50, angle: Double = 40): List[Vector] = {

        val baselineLength = from.length(to).toInt
        val lineBoldness = maxH
        val width = lineBoldness / Math.tan(Math.toRadians(90-angle))
        val width60 = lineBoldness / Math.tan(Math.toRadians(68))

        val normVar = normDot(from, to, baselineLength - width, lineBoldness)
        val norm1 = normDot(from, to, width60, lineBoldness)

        from :: to :: normVar :: norm1 :: Nil
      }


      def drawFigure(dots: List[Vector]): Unit = {
        ctx.beginPath()
        val start = dots.head
        ctx.moveTo(start.x, start.y)

        for(dot <- dots.tail :+ dots.head) {
          ctx.lineTo(dot.x, dot.y)
        }
        ctx.fill()
      }
      def drawLine(from: Vector, to: Vector, frame: Int): Unit = {
        val angle = 1 + 18 * (frame / 100.0)
        val boldness1 = sideLen / 7.0
        val boldness2 = boldness1 / 3.0
        val gap = boldness2 / 2
        val dots = lineToRect(from, to, boldness1, angle)
        //change it to relative
        if(from.length(to) > 30){
          drawFigure(dots)
        }

        val dots1 = lineToRect(dots(3), dots(2), gap, angle)

        val remover = 0.82 * (boldness1 + gap)
        //val remover = 0
        val from1 = dots1(3).pointOnLine(dots1(2), remover)
        val backmover = remover * frame / 99.0
        val to1 = dots1(2).pointOnLine(from1, backmover)

        val dots2 = lineToRect(from1, to1, boldness2, angle)
        if(from1.length(to) > remover * 1.5){
          //drawFigure(dots2)
        }

      }

      def scale(p: Vector): Vector = {
        (p-origin) * 0.7 + origin
      }
      for{
        i <- 0 until dots.length - 1
      }{
        val from = dots(i)
        val to = dots.lift(i+1).getOrElse(dots.head)
//        if(i == 0){
//          ctx.fillStyle = s"rgba(253,164, 40, ${0.7 - state.frame / 100.0})"
//        }else if( i== 1){
//          ctx.fillStyle = s"rgba(253,164, 40, ${1.3 - state.frame / 100.0})"
//        }else {
//          ctx.fillStyle = s"rgba(253,164, 40, 1)"
//        }
        if(i == dots.length - 2){
          drawLine(from, to, state.frame)
        }else {
          drawLine(from, to, 120)
        }

        //drawLine(scale(from), scale(to))
      }
    }

    //dom.window.requestAnimationFrame(drawMe2)




    def drawMeStep(state: SceneState): Unit = {
      drawMe2(state)
      //println(state)
      //dom.window.setTimeout(() => drawMeStep(state.next), 16 * 3)
      //dom.window.requestAnimationFrame()
      //button.onclick = (_) => () => drawMeStep(state.next)
      dom.window.requestAnimationFrame((_) => drawMeStep(state.next))
    }
    drawMeStep(SceneState(0))

  }
}
