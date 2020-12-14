package adventofcode.year2020.day14

import scala.io.Source

object Day14 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("src/adventofcode/year2020/day14/input.txt").getLines().toSeq
    val (_, result) = lines.foldLeft(((0L,0L), Map.empty[Long, Long])) { case((mask, mem), line) =>
      val maskFormat = """mask = (.*)""".r
      val memFormat = """mem\[(\d*)] = (\d*)""".r
      line match {
        case maskFormat(strMask) => (parseMask(strMask), mem )
        case memFormat(addr, value)  => (mask, mem.updated(addr.toLong, applyMask(value.toLong, mask)))
      }
    }
    println(result)
    println(result.map(_._2).sum)

    val (_, result2) = lines.foldLeft(("", Map.empty[Long, Long])) { case((mask, mem), line) =>
      val maskFormat = """mask = (.*)""".r
      val memFormat = """mem\[(\d*)] = (\d*)""".r
      line match {
        case maskFormat(strMask) => (strMask, mem )
        case memFormat(addr, value)  => (mask, mask2Addr(addr.toLong, mask).foldLeft(mem)((m, addr) => m.updated(addr, value.toLong)))
      }
    }
    println(result2)
    println(result2.map(_._2).sum)

  }

  def parseMask(str: String) : (Long, Long) = {
    val r  = (parse(str.replace('X', '0')),
      parse(str.replace('X', '1')))
    println(s"mask=>(${r._1.toBinaryString},${r._2.toBinaryString})")
    r
  }
  def applyMask(value: Long, mask: (Long, Long)): Long = {
    (value | mask._1) & mask._2
  }
  def parse(str: String) : Long = {
    str.toCharArray.foldLeft(0L) { case (n, c)=>
      c match {
        case '0'=> n<<1
        case '1'=> (n << 1) +1
      }
    }
  }

  def mask2Addr(addr: Long, mask: String) : Seq[Long] = {
    val nonFloatingMask = parse(mask.replace('X', '0'))
    val floatingSubMask = parse(mask.replace('0', '1').replace('X', '0'))
    val floatingCombos = floatingMasks(mask.toCharArray).map(parse(_))
    val withNonFloatingMask = addr | nonFloatingMask
    val withFloatingSubMask = withNonFloatingMask & floatingSubMask
    val r = floatingCombos.map(fc => withFloatingSubMask | fc )
    println(s"$mask | ${addr.toBinaryString} => ${withNonFloatingMask.toBinaryString} => ${withFloatingSubMask.toBinaryString} => ${r.map(_.toBinaryString)}")
    r
  }
  def floatingMasks(mask: Array[Char]) : Seq[String] = {
    if(mask.isEmpty) {
      Seq("")
    } else {
      val rest = floatingMasks(mask.tail)
      mask.head match {
        case '0' | '1' => rest.map("0"+_)
        case 'X' => {
          rest.flatMap(r => Seq("1"+r, "0"+r))
        }
      }
    }
  }
}
