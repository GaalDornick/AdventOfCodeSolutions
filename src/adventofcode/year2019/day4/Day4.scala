package adventofcode.year2019.day4

object Day4 {
  val min = 367479
  val max = 893698

  def main(args: Array[String]): Unit = {
    println((min to max).filter(isPassword(_)).length)
  }

  def isPassword(i: Int): Boolean = {
    val password = i.toString.toCharArray
    val lagged = password.zip(password.tail)
    val grouped: Map[Char, Int] = password.groupBy(c => c)
      .map(v => v._1 -> v._2.size)
    lagged.filter(l => l._1>l._2).size==0 && lagged.filter(l => l._1==l._2).size>=1 && grouped.filter(_._2==2).size >=1

  }
}
