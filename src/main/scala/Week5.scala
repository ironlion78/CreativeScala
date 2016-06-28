import doodle.core.Circle
import doodle.core.Line.Cap.Square
import doodle.core._
import doodle.core.Image._
import doodle.syntax._
import doodle.backend.StandardInterpreter._
import doodle.jvm.Java2DCanvas._
import doodle.turtle._
import doodle.turtle.Instruction._

object turtle extends App {

  def polygon(sides: Int, sideLength: Double): Image = {
    val turnangle = Angle.one/sides

    def addInst (n:Int):List[Instruction] = {
      n match{
        case 0 => Nil
        case n => List(forward(sideLength), turn(turnangle)) ++ addInst(n-1)
      }
    }
    val instructions = addInst(sides)
    Turtle.draw(instructions)
  }

  def squareSpiral (sides: Int, initialLength: Int, lenMult: Double): Image = {
    val turnangle = (89.5).degrees

    def addInst (n:Int, len:Int):List[Instruction] = {
      n match{
        case 0 => Nil
        case n => addInst(n-1, len) ++ List(forward(len*Math.pow(lenMult,n)), turn(turnangle))
      }
    }

    val instructions = addInst(sides, initialLength)
    Turtle.draw(instructions)
  }

  def rewrite(instructions: List[Instruction],
              rule: Instruction => List[Instruction]): List[Instruction] = {
    instructions.flatMap(i =>
      i match {
        case Branch(_) => 
        case _ => rule(i)
      }
    )
  }
}

object l extends App {
  def double[A](list:List[A]):List[A] = {
    list.flatMap {x => List(x,x)}
  }
  def nothing[A](list:List[A]):List[A] = {
    list.flatMap {x => Nil}
  }

}