package adventofcode.year2020.day25

object Day25 {
  def main(args: Array[String]): Unit = {
    val input = Seq(9232416, 14144084)
    val inputExample = Seq(5764801, 17807724)

    run(inputExample)
//    println
    run(input)
  }
  def run(publickeys: Seq[Int]) = {

    val loopSizes = publickeys.map(determineLoopSize(_))
    val encryptionKey = calcEncKey(loopSizes.head, publickeys.reverse.head)
    println(s"Enc => ${encryptionKey}")
  }
  val subjectNumber = 7
  val mod = 20201227
  def determineLoopSize(publicKey: Int) : Int = {

    val loops = Stream.from(1).scanLeft(1)((value, _) => {
      (value*subjectNumber)%mod
    }).takeWhile(_!=publicKey).toSeq
    //loops.zipWithIndex.foreach(l => println(s"${l._2+1}) ${l._1}"))
    println(loops.size)
    println("==============")
    loops.size

  }
  def calcEncKey(loopSize: Int, publicKey: Int) = {
    (1 to loopSize).foldLeft(1L)((key, _) => {
      (key*publicKey)%mod
    })
  }
}
