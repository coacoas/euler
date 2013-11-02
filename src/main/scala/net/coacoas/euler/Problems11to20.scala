package net.coacoas.euler
import scala.annotation.tailrec
import scala.io.Source
import Streams._
import java.io.File
import java.io.InputStream

object P11 extends Problem {
  def max(i: Int, j: Int, values: Array[Array[Int]]): Int = {
    def mult(flag: Boolean, f: Int => Int, g: Int => Int) = if (flag)
      (0 until 4).fold(1)((acc, offset) => acc * (values(f(offset))(g(offset))))
    else 0

    def e = mult(j < 16, x => i, x => j + x)
    def s = mult(i < 16, x => i + x, x => j)
    def se = mult(i < 16 && j < 16, x => i + x, x => j + x)
    def sw = mult(i > 4 && j < 16, x => i - x, x => j + x)

    val results = (e :: s :: se :: sw :: Nil)
    results.max
  }

  override def run = {
    val values = Source.fromInputStream(classOf[Problem].getResourceAsStream("/Problem11.dat")).getLines.toArray.map(_.split(' ').map(_.toInt))
    (for {
      i <- 0 until values.length
      j <- 0 until values(i).length
    } yield max(i, j, values)).max
  }
}

object P12 extends Problem {
  override def run = {

    // Much more functional.  Ran about 8 times faster, too. 
    def div2(n: Long) = (1L to math.sqrt(n).toLong).filter(n % _ == 0).flatMap(i => if (n / i == i) List(i) else List(i, n / i))

    // First try
    def divisors(n: Long) = {
      @tailrec
      def nextDivisor(acc: IndexedSeq[Long], last: Long): IndexedSeq[Long] = {
        val next = from(last + 1).find(n % _ == 0).get
        val dividend = n / next
        if (dividend > next) {
          nextDivisor(acc ++ Seq(next, dividend), next)
        } else {
          acc
        }
      }
      nextDivisor(IndexedSeq(), 0)
    }

    triangleNumbers take 10 foreach { n =>
      println("%d %s".format(n, div2(n).sortWith(_ < _).mkString(",")))
    }

    triangleNumbers.dropWhile(n => { div2(n).size < 500 }).head
  }
}

object P13 extends GenericProblem[BigInt] with Timing[BigInt] {
  override def run = Source.fromInputStream(classOf[Problem].getResourceAsStream("/Problem13.dat")).getLines.map(l => BigInt(l.trim)).sum
}

object P14 extends Problem {
  override def run = {
    def seq(l: Long): Stream[Long] = l #:: (if (l == 1) Stream.empty else seq(if (l % 2 == 0) l / 2 else 3 * l + 1))
    (1L until 1000000L).map(n => (n, seq(n).size)).sortWith(_._2 > _._2).head._2
  }
}

object P15 extends GenericProblem[BigInt] with Timing[BigInt]  {
  def grid = for {
    i <- 1 to 21
    j <- 1 to 21
  } yield (i, j)

  def value(m: Map[(Int, Int), BigInt], i: Int, j: Int): BigInt = if (i == 1 || j == 1) 1L else
    (for {
      x <- m.get((i - 1) -> j)
      y <- m.get(i -> (j - 1))
      z <- Option(x + y)
    } yield z).getOrElse(1)

  override def run = {
    val map = grid.foldLeft(Map[(Int, Int), BigInt]())((map, elem) => elem match {
      case (i, j) => map ++ Map((i -> j) -> value(map, i, j))
      case _ => map
    })
    map((21, 21))
  }
}

object P16 extends Problem {
  override def run = (1 to 1000).foldLeft(BigInt(1))((a, b) => a * 2).toString.toList.map(_ - '0').sum
}

object P17 extends Problem {
  def hundreds(i: Int): String = {
    val tens = i % 100
    asString(i / 100) + "hundred" + (if (tens != 0) "and" else "") + asString(tens)
  }

  def asString(i: Int): String = i match {
    case 1000 => "onethousand"
    case 0 => ""
    case 1 => "one"
    case 2 => "two"
    case 3 => "three"
    case 4 => "four"
    case 5 => "five"
    case 6 => "six"
    case 7 => "seven"
    case 8 => "eight"
    case 9 => "nine"
    case 10 => "ten"
    case 11 => "eleven"
    case 12 => "twelve"
    case 13 => "thirteen"
    case 14 => "fourteen"
    case 15 => "fifteen"
    case 16 => "sixteen"
    case 17 => "seventeen"
    case 18 => "eighteen"
    case 19 => "nineteen"
    case j: Int if j >= 20 && j < 30 => "twenty" + asString(j - 20)
    case j: Int if j >= 30 && j < 40 => "thirty" + asString(j - 30)
    case j: Int if j >= 40 && j < 50 => "forty" + asString(j - 40)
    case j: Int if j >= 50 && j < 60 => "fifty" + asString(j - 50)
    case j: Int if j >= 60 && j < 70 => "sixty" + asString(j - 60)
    case j: Int if j >= 70 && j < 80 => "seventy" + asString(j - 70)
    case j: Int if j >= 80 && j < 90 => "eighty" + asString(j - 80)
    case j: Int if j >= 90 && j < 100 => "ninety" + asString(j - 90)
    case j: Int if j >= 100 => hundreds(j)
  }

  override def run = (1 to 1000).map(asString(_).length).sum
}

class p18(filename: String) extends Problem {
  type Triangle = Vector[Vector[Int]]
  case class Position(val row: Int, val column: Int)
  
  def readFile(f: InputStream): Triangle = io.Source.fromInputStream(f).getLines.toVector.map(_.split(" ").map(_.toInt).toVector)

  def run = {
    val tri = readFile(classOf[Problem].getClassLoader().getResourceAsStream(filename))
    val reversed = tri.reverse
    
    reversed.reduce { (totals, row) => 
      row.zipWithIndex.map { case (value, idx) => 
        val maxChild = if (totals(idx) > totals(idx + 1)) totals(idx) else totals(idx + 1)
        value + maxChild
      }
    }.max
  }
}

object P18 extends p18("triangle-18.txt")
object P67 extends p18("triangle-67.txt")

/**
 * You are given the following information, but you may prefer to do some research for yourself.
 *
 * <ul>
 * <li>1 Jan 1900 was a Monday.</li>
 * <li>Thirty days has September,</li>
 * <li>April, June and November.</li>
 * <li>All the rest have thirty-one,</li>
 * <li>Saving February alone,</li>
 * <li>Which has twenty-eight, rain or shine.</li>
 * <li>And on leap years, twenty-nine.</li>
 * <li>A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.</li>
 * </ul>
 *
 * How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
 */
object P19 extends Problem {
  // Add in the extra 0 at the beginning so I can reference months as 1-12 instead of 0-11
  lazy val daysByMonth = Array[Int](0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  def days(month: Int, year: Int): Int = month match {
    case 2 if (year % 4 == 0 && !(year % 100 == 0 && year % 400 != 0)) => 29
    case x => daysByMonth(x)
  }

  @tailrec
  private def rainyDaysAndSundays(acc: Int, curr: Int, month: Int, year: Int): Int = year match {
    case 2001 => acc
    case _ =>
      rainyDaysAndSundays(if (curr == 0) acc + 1 else acc,
        (curr + days(month, year)) % 7,
        (month % 12) + 1,
        if (month == 12) year + 1 else year)
  }

  override def run = rainyDaysAndSundays(0, 0, 1, 1901)
}

/** 
 * n! means n  (n  1)  ...  3  2  1
 * 
 * For example, 10! = 10  9  ...  3  2  1 = 3628800 and the sum of the digits 
 * in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
 * 
 * Find the sum of the digits in the number 100!
 */
object P20 extends Problem {
  def fac(n: Int) = { 
    @tailrec
    def internalfac(acc: BigInt, n: Int): BigInt = n match { 
      case 1 => acc
      case _ => internalfac(acc * n, n - 1)
    }
    internalfac(1, n)
  }
  override def run = { 
	  fac(100).toString.toList.foldLeft(0)((acc, i) => (acc + (i - '0')))
  }
}
