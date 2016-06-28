import doodle.core.Circle
import doodle.core.Line.Cap.Square
import doodle.core._
import doodle.core.Image._
import doodle.syntax._
import doodle.backend.StandardInterpreter._
import doodle.jvm.Java2DCanvas._

object lists extends App {
  def ones (n: Int): List[Int] = {
    n match {
      case 0 => Nil
      case x => 1 :: ones(x-1)
    }
  }

  def descending (n:Int): List[Int] = {
    n match {
      case 0 => Nil
      case x => x :: descending(x-1)
    }
  }


}