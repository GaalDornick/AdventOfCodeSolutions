package adventofcode.year2019.day2

object Day2 {

  val data = IndexedSeq(1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,10,19,2,9,19,23,2,23,10,27,1,6,27,31,1,31,6,35,2,35,10,39,1,39,5,43,2,6,43,47,2,47,10,51,1,51,6,55,1,55,6,59,1,9,59,63,1,63,9,67,1,67,6,71,2,71,13,75,1,75,5,79,1,79,9,83,2,6,83,87,1,87,5,91,2,6,91,95,1,95,9,99,2,6,99,103,1,5,103,107,1,6,107,111,1,111,10,115,2,115,13,119,1,119,6,123,1,123,2,127,1,127,5,0,99,2,14,0,0)

  def main(args: Array[String]): Unit = {

    println(run(data))
    (0 to 99).map { noun =>
      (0 to 99).map { verb =>
        val updatedData = data.updated(1, noun).updated(2, verb)
        val result = run(updatedData)
        if(result == 19690720) {
          println(s"$noun$verb")
        }

      }
    }
  }

  private def run(data: IndexedSeq[Int]) = {
    val idxes = 0 to data.lastIndexOf(99) by 4
    val result = idxes.foldLeft(data) { case (d, idx) =>
      val g = d.slice(idx, idx + 4)
      val invalidIndex = g.tail.filter(n => n < 0 || n >= d.length)
      if (!invalidIndex.isEmpty) {
        throw new Exception(s"${g.mkString(",")} is not a valid command")
      }
      g.head match {
        case 1 => add(d, g)
        case 2 => mult(d, g)
        case 99 => d
        case _ => throw new Exception(s" Wrong opcode ${g.head}")
      }
    }
    result(0)
  }

  def add(d: IndexedSeq[Int], g: IndexedSeq[Int]): IndexedSeq[Int] = {

    d.updated(g(3), d(g(1)) + d(g(2)))
  }
  def mult(d: IndexedSeq[Int], g: IndexedSeq[Int]): IndexedSeq[Int] = {

    d.updated(g(3), d(g(1)) * d(g(2)))
  }

}
