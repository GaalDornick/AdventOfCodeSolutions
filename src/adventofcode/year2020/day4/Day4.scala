package adventofcode.year2020.day4

import scala.io.Source

object Day4 {
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("src/adventofcode/year2020/day4/input.txt").mkString
    val passports = input.split("\n\n")
    val passPortsWithRequiredFIelds = passports.map(parsePassport(_))
        .filter(containsRequiredFields(_))

    println(passPortsWithRequiredFIelds.size)
    val validPassports = passPortsWithRequiredFIelds.filter(validByr(_))
      .filter(validIyr(_))
      .filter(validEyr(_))
      .filter(validHgt(_))
      .filter(validHcl(_))
      .filter(validEcl(_))
      .filter(validPid(_))
    println(validPassports.size)
  }
  def parsePassport(str: String) : Map[String, String] = {
    str.split("[ \n]")
      .map(_.split(":"))
      .map(p => p(0) -> p(1)).toMap
  }
  def containsRequiredFields(vals: Map[String, String]): Boolean = {
    val keys = vals.keys
    val requiredFields = Seq("byr","iyr","eyr","hgt","hcl","ecl","pid")
    requiredFields.intersect(keys.toSeq).size== requiredFields.size
  }
  def validByr(vals: Map[String, String]) = validateYear(vals("byr"), 1920, 2002)
  def validIyr(vals: Map[String, String]) = validateYear(vals("iyr"), 2010, 2020)
  def validEyr(vals: Map[String, String]) = validateYear(vals("eyr"), 2020, 2030)
  def validHgt(vals: Map[String, String]) = {
    val cmFormat = """(\d\d\d)cm""".r
    val inFormat = """(\d\d)in""".r
    vals("hgt") match {
      case cmFormat(cm) => cm.toInt>= 150 && cm.toInt <= 193
      case inFormat(in) => in.toInt>= 59 && in.toInt <= 76
      case _ => false
    }
  }
  def validHcl(vals: Map[String, String]) = {
    val fmt = """#([0-9a-f]{6})""".r
    vals("hcl") match {
      case fmt(_) => true
      case _ => false

    }
  }
  def validEcl(vals: Map[String, String]) = {
    Seq("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(vals("ecl"))
  }
  def validPid(vals: Map[String, String]) = {
    val pidfmt = """(\d{9})""".r
    vals("pid") match {
      case pidfmt(_) => true
      case _ => false
    }
  }

  private def validateYear(v: String, min: Int, max: Int) = {
    val yearFormat = """(\d\d\d\d)""".r
    v match {
      case yearFormat(year) => year.toInt >= min && year.toInt <= max
      case _ => false
    }
  }
}
