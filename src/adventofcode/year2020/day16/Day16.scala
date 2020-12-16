package adventofcode.year2020.day16

import scala.io.Source

object Day16 {

  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("src/adventofcode/year2020/day16/input.txt").getLines()
    val rules = lines.takeWhile(!_.trim.isBlank).toList
      .map(parseRule(_))
    val yourTicket = lines.takeWhile(!_.trim.isBlank).toSeq.tail.head
      .split(",").map(_.toInt).toIndexedSeq
    val otherTickets = lines.toList
      .tail.tail // rest of the tickets
      .map(_.split(",").map(_.toInt).toSeq) // split the line into values
      .map(otherTicket => {
        otherTicket.map(item => (item, validateAgainstRules(item, rules)))// checks each value against each rule and returns a boolean for each value rule
      })


    val result1 = otherTickets.map(otherTicket => {
      otherTicket.map(valueRules => (valueRules._1, valueRules._2._2)) // retursn wherther each value is valid
        .filter(_._2==false) // take only invalid items
        .map(_._1).sum // take the values and sum it
    }).sum
    println(result1)

    val allColumnNames = rules.map(_._1).toSeq
    val numCols = allColumnNames.size
    val validTickets = otherTickets.filter(_.map(_._2._2).reduce(_ && _))// remove all invalid tickets
    val init = Seq.fill(numCols)(allColumnNames)
    val candidateColumnNames = validTickets.map(validTicket => validTicket.map(_._2._1.filter(_._2).map(_._1))) //get the possible column names for each cell
      .foldLeft(init)((acc, candidateCols) => {
        val r = acc.zip(candidateCols).map(c => c._1.intersect((c._2)))
        r
      })

    val targetColumns= candidateColumnNames.zipWithIndex.sortBy(_._1.size)
      .foldLeft(Seq.empty[(String, Int)])((resolved,unresolved) => {
        val col = unresolved._1.filter(!resolved.map(_._1).contains(_))
        println(s"${unresolved._2} ==> ${col.mkString("|")}")
        resolved :+ (col.head, unresolved._2)
      }).filter(_._1.startsWith("departure")).map(_._2)

    println(targetColumns)
    val result2 = targetColumns.map(yourTicket(_)).map(_.toLong)
      .reduce(_ * _)
    println(result2)

  }
  def parseRule(strRule: String) : (String, Seq[(Int, Int)]) = {
    val ruleFormat = """(.*): (\d*)-(\d*) or (\d*)-(\d*)""".r
    strRule match {
      case ruleFormat(name, low1, high1, low2, high2) => (name, Seq((low1.toInt, high1.toInt), (low2.toInt, high2.toInt)))
      case _ => throw new Exception(s"Can't parse $strRule")
    }
  }
  def validateAgainstRules(value: Int, rules: Seq[(String, Seq[(Int, Int)])]):(Seq[(String, Boolean)], Boolean) = {

      val valueRules = rules.map(rule => {
        (rule._1, rule._2.map(range => {
          value >= range._1 && value<=range._2
        }).reduce(_ || _))
      })
    (valueRules, valueRules.map(_._2).reduce(_ || _))
  }
}
