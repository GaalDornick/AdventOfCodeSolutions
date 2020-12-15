package adventofcode.year2020.day15

object Day15 {
  def main(args: Array[String]): Unit = {
    val init = Seq(15,5,1,4,7,0)
    val initWithTurn = init.zipWithIndex.map(n => n._1.toLong -> (n._2+1).toLong).toMap
    val start = System.currentTimeMillis()
    val (_, spoken) = (initWithTurn.size+1L to 30000000L).foldLeft(0L, initWithTurn)({case((spoken, whenSpoken), turn) =>
      if(turn%1000==0) {
        println(turn)
      }
      val nextSpken = turn - whenSpoken.getOrElse(spoken, turn)
      //println((nextSpken, whenSpoken.updated(spoken, turn)))
      (nextSpken, whenSpoken.updated(spoken, turn))
    })
    println("======>"+ spoken.maxBy(_._2)._1)
    val endTime = System.currentTimeMillis()
    println(s"Took ${(endTime-start)/1000.0}s = ${3000000L/((endTime-start)/1000.0)} records per s")
  }
}
