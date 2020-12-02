package adventofcode.year2020.day1

import scala.io.Source

object Day1 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("src/adventofcode/year2020/day1/puzzleinput.txt").getLines().toSeq.map(_.toInt)
    (2 to 3 ).foreach { d=>
      val result = find(2020, lines, d)
      result.orElse({
        println("Not found")
        Some(-1L)
      }).foreach(r => println(s"At depth $d => $r"))
    }

  }

  def find(sum: Int, values: Seq[Int], depth: Int) : Option[Long] = {
    if(depth==0) {
      if(sum==0) {
        Some(1L)
      } else {
        None
      }
    } else if(values.isEmpty) {
      None
    } else {
        val target = sum - values.head
        if(target <0) {
          None
        }
        find(target, values.tail, depth -1 ).map(values.head * _).orElse(find(sum, values.tail, depth))
      }
    }

}
