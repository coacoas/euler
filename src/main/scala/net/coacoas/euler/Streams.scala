package net.coacoas.euler

import java.util.Calendar

object Util {
}

object Streams {
  def from(l: Long): Stream[Long] = l #:: from(l + 1)

  lazy val naturals = from(1)
  
  lazy val fibs: Stream[Long] =  1 #:: 1 #:: (fibs zip fibs.tail).map{ case (a,b) => a+b }

  lazy val triangleNumbers: Stream[Long] = {
    def loop(n: Long, last: Long): Stream[Long] = (n + last) #:: loop(n + 1, n + last)
    loop(1, 0)
  }

  val primes: Stream[Long] = 2L #:: primes.map(i =>
    from(i + 1).find(j =>
      primes.takeWhile(k => k * k <= j).forall(j % _ > 0)).get)

}