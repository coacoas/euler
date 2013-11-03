package net.coacoas.euler

import scala.annotation.tailrec

object P31 extends Problem { 
  def countChange(amount: Int) = { 
    def loop(coins: Vector[Int], remaining: Int): Long = {
      if (remaining == 0) 1
      else if (coins.isEmpty) 0
      else if (coins.head > remaining) loop(coins.tail, remaining)
      else loop(coins, remaining - coins.head) + loop(coins.tail, remaining)
    }
    loop(Vector(200, 100, 50, 20, 10, 5, 2, 1), amount)
  }
  
  override def run = countChange(200)
}

object P34 extends Problem { 
  def digits(n: Int) = n.toString.toList.map(_ - '0')
  def fact(n: Int): BigInt = { 
    @tailrec
    def iter(acc: BigInt, m: Int): BigInt = 
      if (m == 0 || m == 1) acc
      else iter(acc * m, m - 1)
    iter(1, n)
  }
  
  lazy val facts = (0 to 9).map(P34.fact(_)).zipWithIndex.map(_.swap).toMap

  override def run = (3 to 10000000).par.filter(x => digits(x).map(facts).sum == x).sum
}

object P35 extends Problem {
  lazy val primes = Streams.primes.takeWhile (_ < 1000000).toSet
  
  def rotate[T](xs: Seq[T])(n: Int) = xs.slice(n, xs.length) ++ xs.slice(0, n)
  	
  def circulars(n: Long): Seq[Long] = {
    val digits = P34.digits(n.toInt)
    (0 until digits.size).map(rotate(digits)(_).mkString.toLong)
  }
  
  override def run = primes.map(P35.circulars).filter(_.tail.forall(primes)).size
}

object P36 extends Problem { 
  def isPalindrome(s: String) = s == s.reverse
  
  override def run = (0L until 1000000L).filter(x => isPalindrome(x.toString)).filter(y => isPalindrome(y.toBinaryString)).sum
}