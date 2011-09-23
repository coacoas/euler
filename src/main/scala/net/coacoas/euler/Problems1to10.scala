package net.coacoas.euler
import Streams._

class P1Base extends Problem {
  override def run() = naturals.take(999).filter(x => (x % 3 == 0) || (x % 5 == 0)).sum.toString
}
object P1 extends P1Base with Logging with Timing

class P2Base extends Problem {
  override def run = fibs.takeWhile(_ < 4000000).filter(_ % 2 == 0).sum.toString
}
object P2 extends P2Base with Timing with Logging

class P3Base extends Problem {
  override def run = {
    var theNum = 600851475143L
    naturals.drop(1).dropWhile(n => { while (theNum % n == 0) { theNum /= n }; theNum > 1 }).head.toString
  }
}
object P3 extends P3Base with Timing with Logging

class P4Base extends Problem {
  override def run = {
    for {
      i <- 100 to 999
      j <- 100 to 999
      if ((i * j).toString.reverse == (i * j).toString)
    } yield (i * j)
  }.max.toString
}
object P4 extends P4Base with Timing with Logging

class P5Base extends Problem {
  val all = naturals.take(20).toSeq
  override def run = Iterator.from(1).find(i => all.forall(d => i % d == 0)).toString()
} 
object P5 extends P5Base with Timing with Logging

class P6Base extends Problem {
  def sumOfSquares(longs: Seq[Int]) = longs.foldLeft(0)((acc, l) => acc + (l * l))
  def squareOfSums(longs: Seq[Int]) = { val sums = longs.sum; sums * sums } 

  override def run = (squareOfSums((1 to 100).toSeq) - sumOfSquares((1 to 100).toSeq)).toString
}
object P6 extends P6Base with Timing with Logging

class P7Base extends Problem { 
  override def run = primes.drop(10000).head.toString
}
object P7 extends P7Base with Timing with Logging

class P8Base extends Problem {
  override def run = {
    """73167176531330624919225119674426574742355349194934
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
""".replaceAll("""\n""", "").map(_ - '0').sliding(5).map(x => x.foldLeft(1)(_ * _)).max.toString
  }
}
object P8 extends P8Base with Timing with Logging

class P9Base extends Problem {
  override def run = "Test"
}
object P9 extends P9Base with Timing with Logging

class P10Base extends Problem {
  override def run = primes.takeWhile(_ < 2000000).sum.toString
}
object P10 extends P10Base with Timing with Logging
