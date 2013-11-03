/**
 *
 */
package net.coacoas.euler

import net.coacoas.euler._
import scala.io.Source
import Streams._

/**
 * Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
 * If d(a) = b and d(b) = a, where a  b, then a and b are an amicable pair and each of a and b are called
 * amicable numbers.
 *
 * For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284.
 * The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
 *
 * Evaluate the sum of all the amicable numbers under 10000.
 */
object P21 extends Problem {
  def d(n: Int) = divisors(n).sum
  override def run = (1 to 10000).filter(x => { 
	  val d1 = d(x)
	  d1 != x && x == d(d1)
  }).sum
}

/**
 * Using names.txt (Saved as a resource), a 46K text file containing over
 * five-thousand first names, begin by sorting it into alphabetical order. Then working out the
 * alphabetical value for each name, multiply this value by its alphabetical position in the list
 * to obtain a name score.
 *
 * For example, when the list is sorted into alphabetical order, COLIN, which is worth
 * 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a
 * score of 938  53 = 49714.
 *
 * What is the total of all the name scores in the file?
 */
object P22 extends Problem  {
  def unquote(s: String) = if (s.startsWith("\"")) s.substring(1, s.length - 1) else s
  def names = Source.fromInputStream(classOf[Problem].getResourceAsStream("/names.txt")).
    mkString.split(",").map(unquote).sorted
  def score(name: String) = name.toSeq.map(_ - 'A' + 1).sum

  override def run = names.zipWithIndex.map{case (name, i) => (i + 1) * score(name)}.sum
}

/**
A perfect number is a number for which the sum of its proper divisors is exactly equal to the number. For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant if this sum exceeds n.

As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as the sum of two abundant numbers is 24. By mathematical analysis, it can be shown that all integers greater than 28123 can be written as the sum of two abundant numbers. However, this upper limit cannot be reduced any further by analysis even though it is known that the greatest number that cannot be expressed as the sum of two abundant numbers is less than this limit.

Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.
 */
object P23 extends GenericProblem[BigInt] with Timing[BigInt] {
  lazy val abundantMap = abundantNums.takeWhile(_ < 28123).foldLeft(Map[Int, Int]()) { (acc, i) => acc + (i -> 1) }

  def isSumOfAbundants(n: Int) = {
    val lesser = abundantNums.takeWhile(_ <= n/2)
    lesser.exists(x => abundantMap.contains(n - x))
  }

	override def run = (BigInt(0) /: (1 to 28123).filter(x => 
    !isSumOfAbundants(x))) { (acc, i) => acc + i }
}

/**
A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation of the digits 1, 2, 3 and 4. If all of the permutations are listed numerically or alphabetically, we call it lexicographic order. The lexicographic permutations of 0, 1 and 2 are:

012   021   102   120   201   210

What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
*/
object P24 extends Problem {
  override def run = (0 to 9).toVector.permutations.take(1000000).map(_.mkString).toVector.sortWith(_ > _).head.toLong
}

/**
The Fibonacci sequence is defined by the recurrence relation:

Fn = Fn1 + Fn2, where F1 = 1 and F2 = 1.
Hence the first 12 terms will be:

F1 = 1
F2 = 1
F3 = 2
F4 = 3
F5 = 5
F6 = 8
F7 = 13
F8 = 21
F9 = 34
F10 = 55
F11 = 89
F12 = 144
The 12th term, F12, is the first term to contain three digits.

What is the first term in the Fibonacci sequence to contain 1000 digits?
 */ 
 object P25 extends GenericProblem[BigInt] with Timing[BigInt] { 
   def firstTermWithDigits(n: Int) = fibs.zipWithIndex.find { case (value, idx) => value.toString.size >= n }.get._2 + 1
   override def run = firstTermWithDigits(1000)
 }
 
 /**
  * A unit fraction contains 1 in the numerator. The decimal representation 
  * of the unit fractions with denominators 2 to 10 are given:
  * 
  * 1/2	= 	0.5
  * 1/3	= 	0.(3)
  * 1/4	= 	0.25
  * 1/5	= 	0.2
  * 1/6	= 	0.1(6)
  * 1/7	= 	0.(142857)
  * 1/8	= 	0.125
  * 1/9	= 	0.(1)
  * 1/10	= 	0.1
  * Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 
  * 1/7 has a 6-digit recurring cycle.
  * 
  * Find the value of d < 1000 for which 1/d contains the longest recurring cycle in
  * its decimal fraction part.
  */
 object P26 extends Problem { 
   def gcd(m: Int, n: Int) = if (n == 0) m else (n, m % n)
   def coprime(m: Int, n: Int) = gcd(m, n) == 1
   
   override def run = ???
 }


 /**
 Consider all integer combinations of ab for 2  a  5 and 2  b  5:

22=4, 23=8, 24=16, 25=32
32=9, 33=27, 34=81, 35=243
42=16, 43=64, 44=256, 45=1024
52=25, 53=125, 54=625, 55=3125
If they are then placed in numerical order, with any repeats removed, we get the following sequence of 15 distinct terms:

4, 8, 9, 16, 25, 27, 32, 64, 81, 125, 243, 256, 625, 1024, 3125

How many distinct terms are in the sequence generated by ab for 2  a  100 and 2  b  100?
  */
object P29 extends Problem {
  def powers(max: Int) = for {
    base <- 2 to max
    exponent <- 2 to max
  } yield BigInt(base).pow(exponent)
  override def run = powers(100).distinct.size
}

/**
 */ 
object P30 extends Problem {
  def pow(base: Long, exponent: Int) = (1 to exponent).foldLeft(1L){ (acc, i) => acc * base }
  def digits(n: Long) = n.toString.toList.map(_ - '0')
  def toFifth(n: Long) = digits(n).map(x => pow(x, 5))
 
  override def run = (for { 
    i <- 2L to 999999L
    if (i == toFifth(i).sum)
  } yield i).sum
}