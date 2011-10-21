package net.coacoas.euler
import Streams._

object P1 extends GenericProblem[Long] with Timing[Long] {
  override def run = naturals.take(999).filter(x => (x % 3 == 0) || (x % 5 == 0)).sum
}

object P2 extends Problem with Timing[Long] {
  override def run = fibs.takeWhile(_ < 4000000).filter(_ % 2 == 0).sum
}

object P3 extends Problem with Timing[Long] {
  override def run = {
    var theNum = 600851475143L
    naturals.drop(1).dropWhile(n => { while (theNum % n == 0) { theNum /= n }; theNum > 1 }).head
  }
}

object P4 extends Problem with Timing[Long] {
  override def run = {
    for {
      i <- 100 to 999
      j <- 100 to 999
      if ((i * j).toString.reverse == (i * j).toString)
    } yield (i * j)
  }.max
}

object P5 extends Problem with Timing[Long] {
  val all = naturals.take(20).toSeq
  override def run = Iterator.from(1).find(i => all.forall(d => i % d == 0)).get
}

object P6 extends Problem with Timing[Long] {
  def sumOfSquares(longs: Seq[Int]) = longs.foldLeft(0)((acc, l) => acc + (l * l))
  def squareOfSums(longs: Seq[Int]) = { val sums = longs.sum; sums * sums }

  override def run = (squareOfSums((1 to 100).toSeq) - sumOfSquares((1 to 100).toSeq))
}

object P7 extends Problem with Timing[Long] {
  override def run = primes.drop(10000).head
}

object P8 extends Problem with Timing[Long] {
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
""".replaceAll("""\n""", "").map(_ - '0').sliding(5).map(x => x.foldLeft(1)(_ * _)).max
  }
}

object P9 extends Problem with Timing[Long] {
  override def run = ???
}

object P10 extends Problem with Timing[Long] {
  override def run = primes.takeWhile(_ < 2000000).sum
}
