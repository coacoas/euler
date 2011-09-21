package net.coacoas

import Stream._
import scala.annotation.tailrec
import java.util.Calendar

object euler extends App {
  def from(l: Long): Stream[Long] = l #:: from(l + 1)

  def time[T](f: () => T) = {
    val start = Calendar.getInstance
    val s = f()
    val end = Calendar.getInstance
    println(s)
    println("Took %d ms".format(end.getTimeInMillis() - start.getTimeInMillis()))
  }

  object primes {
    val values: Stream[Long] = 2L #:: values.map(i =>
      from(i + 1).find(j =>
        values.takeWhile(k => k * k <= j).forall(j % _ > 0)).get)
  }

  p12
  def p12 = {
    lazy val tri: Stream[Long] = {
      def loop(n: Long, last: Long): Stream[Long] = (n + last) #:: loop(n + 1, n + last)
      loop(1, 0)
    }

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

    tri take 10 foreach { n =>
      println("%d %s".format(n, div2(n).sortWith(_ < _).mkString(",")))
    }

    time(() => tri.dropWhile(n => { div2(n).size < 500 }).head)
  }
  
  def p10 = println(primes.values.takeWhile(_ < 2000000).sum)

  def p8 = {
    val l = """73167176531330624919225119674426574742355349194934
96983520312774506326239578318016984801869478851843
85861560789112949495459501737958331952853208805511
12540698747158523863050715693290963295227443043557
66896648950445244523161731856403098711121722383113
62229893423380308135336276614282806444486645238749
30358907296290491560440772390713810515859307960866
70172427121883998797908792274921901699720888093776
65727333001053367881220235421809751254540594752243
52584907711670556013604839586446706324415722155397
53697817977846174064955149290862569321978468622482
83972241375657056057490261407972968652414535100474
82166370484403199890008895243450658541227588666881
16427171479924442928230863465674813919123162824586
17866458359124566529476545682848912883142607690042
24219022671055626321111109370544217506941658960408
07198403850962455444362981230987879927244284909188
84580156166097919133875499200524063689912560717606
05886116467109405077541002256983155200055935729725
71636269561882670428252483600823257530420752963450
""".replaceAll("""\n""", "").map(_ - '0')

    println(l.sliding(5).map(_.product).max)
  }
}