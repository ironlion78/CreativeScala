import doodle.core.Circle
import doodle.core.Line.Cap.Square
import doodle.core._
import doodle.core.Image._
import doodle.syntax._
import doodle.backend.StandardInterpreter._
import doodle.jvm.Java2DCanvas._

object shapes extends App {

  def concentricCircles(nCircles: Int): Image = {
    nCircles match {
      case 0 => Image.empty
      case n => concentricCircles(n-1) on (circle(10 * n) fillColor Color.red.spin((100*n).degrees))
    }
  }

  def sierpinsky (nTris: Int): Image = {
    nTris match {
      case 0 => triangle(10,10)
      case n => (sierpinsky(n-1) fillColor Color.red.spin((100*n).degrees)) above ((sierpinsky(n-1) fillColor Color.red.spin((100*n).degrees) beside sierpinsky(n-1)) fillColor Color.red.spin((100*n).degrees))
    }
  }

  def chessboard (len: Int, nQuads: Int): Image = {
    nQuads match {
      case 0 => (rectangle(len,len) fillColor Color.black beside rectangle(len,len) fillColor Color.white) above (rectangle(len,len) fillColor Color.white beside rectangle(len,len) fillColor Color.black)
      case n => (chessboard(len, n-1) beside chessboard(len, n-1)) above (chessboard(len, n-1) beside chessboard(len, n-1))
    }
  }

  def paramPoint(a: Int): Point = {
    Point.polar(100.0, a.degrees)
  }

  def paramCircle(degrees: Int): Image = {
    degrees match{
      case 0 => Image.empty
      case n => circle(5).at(paramPoint(degrees).toVec) on paramCircle(n-1)
    }
  }

  def scaledPointMaker (factor: Double): (Point => Point) = {
    (p: Point) => Point.polar(p.r * factor, p.angle)
  }

  //refactor paramCircle to use scaled sizes and also make it generic
}
