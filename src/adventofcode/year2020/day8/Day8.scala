package adventofcode.year2020.day8

import scala.annotation.tailrec
import scala.io.Source

object Day8 {

  def main(args: Array[String]): Unit = {
    val commands = Source.fromFile("src/adventofcode/year2020/day8/input.txt").getLines().toSeq
      .map(parse(_))


    val acc = runCommand(commands, 0, 0)
    println(acc._1)

    val bruteForceRUns = commands.zipWithIndex
        .filter(cmd => Seq("jmp", "nop").contains(cmd._1._1))
      .map({command =>
        command match {
          case (("jmp", n, _), i) => runCommand(commands.updated(i, ("nop", n, false)), 0, 0)
          case (("nop", n, _), i) => runCommand(commands.updated(i, ("jmp", n, false)), 0, 0)
        }
      })
    val result2 =  bruteForceRUns
      .filter({run => // keep only runs that have reached the end
        run._2.reverse.head._3
      }).map(_._1)// get the acc
      .head // just take the first one

    println(result2)

  }
  @tailrec
  private def runCommand(commands: Seq[(String, Int, Boolean)], acc: Int, ptr: Int): (Int, Seq[(String, Int, Boolean)]) = {
    if(ptr >=commands.length || commands(ptr)._3==true) {
      (acc, commands)
    } else {
      val curr = commands(ptr)
      val (newAcc, newPtr) = curr match {
        case ("acc", n, false) => (acc+n, ptr+1)
        case("jmp", n, false) => (acc, ptr+n)
        case("nop", n, false) => (acc, ptr+1)
      }
      runCommand(commands.updated(ptr, (curr._1, curr._2, true)), newAcc, newPtr)
    }
  }
  def parse(str: String): (String, Int, Boolean) = {
    val format = """(.*) ([+-]\d*)""".r
    str match {
      case format(action, amt) => (action, amt.toInt, false)
    }
  }
}
