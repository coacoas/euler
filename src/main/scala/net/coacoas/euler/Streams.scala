package net.coacoas.euler

import java.util.Calendar

object Util {
}

object Streams {
  def from(l: Long): Stream[Long] = l #:: from(l + 1)

  lazy val naturals = from(1)
  
  lazy val fibs: Stream[BigInt] =  1 #:: 1 #:: (fibs zip fibs.tail).map{ case (a,b) => a+b }

  lazy val triangleNumbers: Stream[Long] = {
    def loop(n: Long, last: Long): Stream[Long] = (n + last) #:: loop(n + 1, n + last)
    loop(1, 0)
  }

  def divisors(n: Int): Seq[Int] = (1 to n/2).filter(x => n % x == 0)
  def isAbundant(n: Int) = divisors(n).sum > n
  def isPerfect(n: Int) = divisors(n).sum == n
  val abundantNums: Stream[Int] = Stream.from(1).filter(isAbundant)


  val primes: Stream[Long] = 2L #:: primes.map(i =>
    from(i + 1).find(j =>
      primes.takeWhile(k => k * k <= j).forall(j % _ > 0)).get)
}