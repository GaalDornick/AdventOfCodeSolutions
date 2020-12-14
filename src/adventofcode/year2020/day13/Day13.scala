package adventofcode.year2020.day13

import scala.io.Source

object Day13 {

  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("src/adventofcode/year2020/day13/input.txt").getLines().toSeq
    val earliestTime = lines(0).toLong
    val buses = lines(1).split(",").toSeq
    val minsToWait = buses.filter(_!="x")
      .map(_.toInt)
      .map(id => (id, id - earliestTime %id))
      .minBy(_._2)
    println((minsToWait._1 * minsToWait._2))

    val busesWithIndex = buses.zipWithIndex.filter(_._1!="x").map(b => (b._1.toInt, b._2)).sortBy(_._1).reverse
    val maxId = busesWithIndex.head
    val result = Stream.continually(1).scanLeft((false, maxId._1.toLong - maxId._2.toLong, maxId._1.toLong, Seq(maxId), busesWithIndex.tail)){case ((done, t, inc, resolved, remaining), _) =>
      println(s"==>$t, $inc, $resolved, $remaining")
      if(!remaining.isEmpty) {
        val matched = remaining.filter(r => (t+r._2)%r._1==0)
        val newInc = inc*matched.map(_._1).foldLeft(1)(_*_)
        (false, t+newInc, newInc, resolved ++ matched, remaining.filter(!matched.contains(_)))
      } else {
        (true, t , inc, resolved , remaining)
      }
    }.takeWhile(e=> !e._1).reverse.head
    println((result._2-result._3))

  }
}
