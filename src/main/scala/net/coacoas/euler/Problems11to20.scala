package net.coacoas.euler
import scala.annotation.tailrec
import scala.io.Source

import Streams._

class P11Base extends Problem[Long] {
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

  def run = {
    val values = Source.fromInputStream(classOf[P11Base].getResourceAsStream("/Problem11.dat")).getLines.toArray.map(_.split(' ').map(_.toInt))
    (for {
      i <- 0 until values.length
      j <- 0 until values(i).length
    } yield max(i, j, values)).max
  }
}
object P11 extends P11Base with Timing[Long] with Logging[Long]

class P12Base extends Problem[String] {
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

    triangleNumbers.dropWhile(n => { div2(n).size < 500 }).head.toString
  }
}
object P12 extends P12Base with Timing[String] with Logging[String]

class P13Base extends Problem[BigInt] {
  override def run = Source.fromInputStream(classOf[P13Base].getResourceAsStream("/Problem13.dat")).getLines.map(l => BigInt(l.trim)).sum
}
object P13 extends P13Base with Timing[BigInt] with Logging[BigInt]

class P14Base extends Problem[String] {
  override def run = {
    def seq(l: Long): Stream[Long] = l #:: (if (l == 1) Stream.empty else seq(if (l % 2 == 0) l / 2 else 3 * l + 1))
    (1L until 1000000L).map(n => (n, seq(n).size)).sortWith(_._2 > _._2).head.toString
  }
}
object P14 extends P14Base with Timing[String] with Logging[String]

class P15Base extends Problem[BigInt] {
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

  def run = {
    val map = grid.foldLeft(Map[(Int, Int), BigInt]())((map, elem) => elem match {
      case (i, j) => map ++ Map((i -> j) -> value(map, i, j))
      case _ => map
    })
    map((21, 21))
  }
}
object P15 extends P15Base with Timing[BigInt] with Logging[BigInt]

class P16Base extends Problem[BigInt] {
  override def run = (1 to 1000).foldLeft(BigInt(1))((a, b) => a * 2).toString.toList.map(_ - '0').sum
}
object P16 extends P16Base with Timing[BigInt] with Logging[BigInt]

class P17Base extends Problem[Long] {
  def hundreds(i: Int): String = {
    val tens = i % 100
    asString(i / 100) + "hundred" + (if (tens!= 0) "and" else "") + asString(tens)
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
object P17 extends P17Base with Timing[Long] with Logging[Long]

class P18Base extends Problem[Long] {
  def run = 0
}
object P18 extends P18Base with Timing[Long] with Logging[Long] 