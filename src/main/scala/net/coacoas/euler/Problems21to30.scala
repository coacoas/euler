/**
 *
 */
package net.coacoas.euler

import net.coacoas.euler._
import scala.io.Source

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
object P21

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