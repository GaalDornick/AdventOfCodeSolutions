package adventofcode.year2020.day23

object Day23 {

  val strInputExample = "389125467"
  val movesExample = 10
  val strInput = "215694783"
  val moves = 100

  type Cup =  Int
  type Cups = Array[Cup]
//  class Cups(val _list: Seq[Either[Int, Range.Inclusive]]) {
//    val _indices: Seq[(Int,Int)] = {
//      _list.scanLeft((-1, 0))((idxPrevItem, item) => {
//        val idxItem = item match {
//          case Left(_) => (idxPrevItem._2, idxPrevItem._2+1)
//          case Right(r) => (idxPrevItem._2, idxPrevItem._2+r.size)
//        }
//        idxItem
//      }).tail
//    } //(0 until fixedNums.length).sliding(1).map(t => (t(0), t(1))).toSeq :+ (fixedNums.length, fixedNums.length+range.length)
//    def length = _list.map(_ match {
//      case Left(_) => 1
//      case Right(b) => b.length
//    }).sum
//
//    def indexOf(i: Cup):Int = {
//      val found = _list.zipWithIndex.find(_._1 match {
//        case Left(a) => a==i
//        case Right(b) => b.contains(i)
//      })
//      found.map(_ match {
//        case (Left(_), n) => _indices(n)._1
//        case(Right(r), n) => _indices(n)._1 + i - r.start
//      }).getOrElse(-1)
//    }
//    def contains(i: Cup): Boolean = {
//      val idx = indexOf(i)
//      idx != -1
//    }
//    def apply(i: Cup) = get(i)
//    def get(idx: Int): Int = {
//      val found = _indices.zipWithIndex.find(i => i._1._1<=idx && i._1._2>idx)
//      found.map( f => {
//        val ((start, _), index) = f
//        _list(index) match {
//          case Left(a) => a
//          case Right(b) => b.start + idx - start
//        }
//      }).getOrElse(-1)
//    }
//
//    def prettyPrint(currentCup: Cup): String = {
//      _list.map(_ match {
//        case Left(a) if a == currentCup => s"(${currentCup})"
//        case Left(other) => s"$other"
//        case Right(r) if (r.start<=currentCup && r.end>currentCup) => s"(${r.start}-${r.end})"
//        case Right(r)  => s"${r.start}-${r.end}"
//      }).mkString(" ")
//    }
//    def remove(pickUp: IndexedSeq[Cup]): Cups = {
//      pickUp.foldLeft(this)((cups, p) => {
//        cups.remove(p)
//      })
//    }
//    def remove(cup:Cup) : Cups = {
//      val _newlist: Seq[Either[Int, Range.Inclusive]] = _list.flatMap(item => {
//        val entries : Seq[Either[Int, Range.Inclusive]] = item match {
//          case Left(a) => if (a == cup) Seq.empty[Either[Int, Range.Inclusive]] else Seq(Left(a))
//          case Right(b) => if (b.contains(cup)) {
//            val r = (b.start, b.end) match {
//              case (_, ce) if(ce==cup+1) => Seq(Right((b.start to cup - 1)))
//              case (ce, _) if(ce==cup) => Seq(Right((cup + 1 to b.end)))
//              case (_, _) => Seq(Right((b.start to cup - 1)), Right((cup + 1 to b.end)))
//            }
//            r.map(_ match {
//              case Right(b) if(b.start==b.end) => Left(b.start)
//              case other => other
//            })
//          } else {
//            Seq(Right(b))
//          }
//        }
//        entries
//      })
//      new Cups(_newlist)
//    }
//
//    def max = {
//      _list.map(_ match {
//        case Left(a) => a
//        case Right(b) => b.end
//      }).max
//    }
//
//    def slice(start: Int, end: Int): Cups = {
//      val items = _indices.zip(_list).flatMap(itemWithIdx => {
//        val(idx, item) = itemWithIdx
//        if(idx._2 <= start || idx._1 >= end) {
//          // item out of range don't return
//          Seq.empty[Either[Int, Range.Inclusive]]
//        } else {
//          item match {
//            case Left(a) => Seq(Left(a))
//            case Right(b) => {
//              val rStart = if(start > idx._1) b.start+start-idx._1 else b.start
//              val rEnd = if(end < idx._2) b.end - (end - idx._2) else b.end
//              Seq(Right((rStart to rEnd)))
//            }
//          }
//        }
//      })
//      new Cups(items)
//    }
//
//    def ++(other: Cups): Cups = {
//      new Cups(_list ++ other._list)
//    }
//  }

//  object Cups {
//    def apply(fixedNums: Seq[Cup], range: Range.Inclusive ) : Cups = {
//      val _list: Seq[Either[Int, Range.Inclusive]] = fixedNums.map(Left(_)) :+ Right(range)
//      new Cups(_list)
//    }
//    def apply(fixedNums: Seq[Cup]) : Cups = {
//      val _list: Seq[Either[Int, Range.Inclusive]] = fixedNums.map(Left(_))
//      new Cups(_list)
//    }
//  }


  object Cups {
    def apply(fixedNums: Seq[Cup], range: Range.Inclusive ) : Cups = {
      (fixedNums ++ range).toArray
    }
    def apply(fixedNums: Seq[Cup]) : Cups = {
      fixedNums.toArray
    }
  }

  class CupsHelper(cups: Cups) {
    def prettyPrint(currentCup: Cup) : String = {
//            cups.map(_ match {
//              case a if a == currentCup => s"(${currentCup})"
//              case other => s"$other"
//            }).mkString(" ")
      ""
    }
    def remove(cupsToRemove: Seq[Cup]): Cups = {

      cups.filter(!cupsToRemove.contains(_))
    }
  }

  implicit def cupsToCupsHelper(cups: Cups) = new CupsHelper(cups)



  def main(args: Array[String]): Unit = {
    val inputExample = strInputExample.toCharArray.map(n => s"$n".toInt).toSeq
    val input = strInput.toCharArray.map(n => s"$n".toInt).toSeq
    //run( Cups(inputExample), movesExample)
    //run( Cups(input), moves)

    run(Cups(input ,(inputExample.max+1 to 1000000)), 1000)
    //run(input ++ (input.max+1 to 1000000), moves)
  }
  def log(str: String) = {
    //println(str)
  }

  def run(input: Cups, moves: Int) = {
    val atartTime = System.currentTimeMillis()
    val (finalCups, finalIndex) =  (1 to moves).foldLeft((input, -1))((acc, moveNum) => {
      val(cups, prevIndex) = acc
      val nextIndex = if(prevIndex==cups.length-1) 0 else prevIndex+1
//      if(moveNum%50==0) {
//        println(s"$moveNum)")
//      }
      move(cups, nextIndex , moveNum)
    })
    val timeTaken = (System.currentTimeMillis() - atartTime)
    println(s"TIme taken for $moves moves of size ${input.length} is ${timeTaken}ms - ${timeTaken/moves}ms/move; EstimatedTime for full soln ${timeTaken/60.0}s")
    println("-- final --")
    val currentCup = finalCups(finalIndex)
//    val prettyCups = finalCups.map(cup => if(cup==currentCup) s"($cup)" else s"$cup").mkString(" ")
   // println(finalCups.prettyPrint(currentCup))

    val  idxResult = (finalCups.indexOf(1) +1)%finalCups.length

//    val result = finalCups.slice(indexOf1+1, finalCups.length ) ++ finalCups.slice(0, indexOf1)
//    println(result.mkString)
    println(s"${finalCups(idxResult)} * ${finalCups(idxResult+1)}")
  }
  def move(oldCups: Cups, currentIndex: Int, moveNum: Int): (Cups, Int) = {
    log(s"-- move $moveNum --")

    //rotate the cups so current cup is in front
    val cups = Array.ofDim[Cup](oldCups.size)
    Array.copy(oldCups, currentIndex, cups, 0, oldCups.length - currentIndex)
    Array.copy(oldCups, 0, cups, oldCups.length - currentIndex, currentIndex)

    val currentCup = cups(0)
    log(cups.prettyPrint(currentCup))

    val pickup = Array.ofDim[Cup](3)
    Array.copy(cups, 1, pickup, 0, 3)

    val insertCup = if(pickup(0)!=(currentCup-1) && pickup(0)!=(currentCup-1) && pickup(0)!=(currentCup-1)) {
      currentCup - 1
    } else if(pickup(0)!=(currentCup-2) && pickup(0)!=(currentCup-2) && pickup(0)!=(currentCup-2)) {
      currentCup - 2
    } else if(pickup(0)!=(currentCup-3) && pickup(0)!=(currentCup-3) && pickup(0)!=(currentCup-3)) {
      currentCup - 3
    } else currentCup - 4

    val insertIndex = cups.indexOf(insertCup)


    Array.copy(cups, 4, cups, 1, insertIndex-3)
    Array.copy(cups, insertIndex+1, pickup, 0, 3)





    //    val currentCup = cups(currentIndex)
////    val prettyCups = cups.map(cup => if(cup==currentCup) s"($cup)" else s"$cup").mkString(" ")
////    log(prettyCups)
//    log(cups.prettyPrint(currentCup))
//
//    //**64 s
//    //val pickupIndexes = IndexedSeq(1,2, 3).map(n => (n, (currentIndex+n)%cups.length)).sortBy(_._1).map(_._2)
//    val firstPickupIndex = (currentIndex+1)%cups.length
//    //**0s
//    //val pickUp = pickupIndexes.map(cups(_))
//    //log(s"pick up: ${pickUp.mkString(", ")}")
//    //**0s
//    //val remaining = cups.remove(pickUp)
//
//    //16s
//    val(pickup, remaining) = firstPickupIndex match {
//      case n if n==cups.length-1 => (Seq(cups(n), cups(0), cups(1)), cups.slice(3, cups.length-1))
//      case n if n==cups.length-2 => (Seq(cups(n), cups(n+1), cups(0)), cups.slice(2, cups.length-2))
//      case n  => (Seq(cups(n), cups(n+1), cups(n+2)), cups.slice(0, n) ++ cups.slice(n+1, cups.length))
//
//    }
//    //30s
//    //val remaining = cups.filter(c => c!=pickUp(0)&&c!=pickUp(1)&&c!=pickUp(2))
//
//
//    //** 0
//    val destination: Cup = Stream.from(1).map(offset => {
//
//      val dest = currentCup - offset
//
//      if(dest <=0) cups.max + dest
//      else dest
//    }).takeWhile(!remaining.contains(_)).reverse.headOption.getOrElse(currentCup)-1
//
//
//
//    val dest  = if(destination ==0) cups.max else destination
//    //*/
//
////    log(s"destination: $dest")
//
////    log("")
///**16s
//    val destIndex = remaining.indexOf(dest)
//    val next = remaining.slice(0, destIndex+1) ++
//      Cups(pickUp) ++
//      remaining.slice(destIndex+1, remaining.length)
//    (next, next.indexOf(currentCup))
// **/
    (cups, 1)

  }
}
