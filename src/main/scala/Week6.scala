import doodle.core.Circle
import doodle.core.Line.Cap.Square
import doodle.core._
import doodle.core.Image._
import doodle.syntax._
import doodle.backend.StandardInterpreter._
import doodle.jvm.Java2DCanvas._
import doodle.random._
import cats.syntax.cartesian._

object r extends App {

  def randomAngle: Random[Angle] =
    Random.double.map(x => x.turns)
  // randomAngle: doodle.random.Random[doodle.core.Angle]
  def randomColor(s: Normalized, l: Normalized): Random[Color] =
    randomAngle map (hue => Color.hsl(hue, s, l))
  // randomColor: (s: doodle.core.Normalized, l: doodle.core.Normalized)doodle.random.Random[doodle.core.Color]
  def randomCircle(r: Double, color: Random[Color]): Random[Image] =
    color map (fill => Image.circle(r) fillColor fill)


  def randomConcentricCircles(n: Int): Random[Image] = {

    val randomPastel = randomColor(0.7.normalized, 0.7.normalized)
    n match {
      case 0 => randomCircle(10, randomPastel)
      case n =>
        randomConcentricCircles(n-1) |@| randomCircle(n * 10, randomPastel) map {
          (circles, circle) => circles on circle
        }
    }
  }

  def colorBox(l: Double, color: Color): Image = {
    Image.rectangle(l,l) fillColor color
  }

  def randomBoxes (n: Int): Random[Image] = {
    val randomPastel = randomColor(0.7.normalized, 0.7.normalized)
    n match {
      case 0 => randomPastel map {c => colorBox(10, c)}
      case n =>
      (randomBoxes(n-1) |@| (randomPastel map {c => colorBox(10, c)})) map {
          (boxes, box) => box beside boxes
        }
    }
  }

  def gradientBoxes(n: Int, color: Color): Image = {
    val l = 10
    n match {
      case 0 => colorBox(l, color)
      case n => colorBox(l, color) beside gradientBoxes(n - 1, color.spin(15.degrees))
    }
  }

  def twistColor (color:Color): Random[Color] = {
    val twist = Random.normal(10.0, 10.0)
    twist map {t => color.spin(t.degrees)}
  }

  def twistedGradientBoxes (n: Int, color: Color): Random[Image] = {
    n match {
      case 0 => twistColor(color) map {c => colorBox(10, c)}
      case n =>
        ( (twistColor(color) map {c => colorBox(10, c)}) |@| twistedGradientBoxes(n-1, color.spin(15.degrees)) ) map {
          (box, boxes) => box beside boxes
        }
    }
  }
}
