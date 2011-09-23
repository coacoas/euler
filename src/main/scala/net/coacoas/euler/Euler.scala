package net.coacoas.euler

import scala.annotation.tailrec
import java.util.Calendar

abstract class Problem {
  def run: String
}

abstract class ActiveProblem extends Problem { 
  run
}

trait Timing extends Problem {
  def time[T](f: () => T): T = {
    val start = Calendar.getInstance.getTimeInMillis()
    val s = f()
    val end = Calendar.getInstance.getTimeInMillis()
    println("Took %d ms".format(end - start))
    s
  }

  abstract override def run = time (super.run _) 
}

trait Logging extends Problem {
  abstract override def run = {
    lazy val result = super.run
    println(result)
    result
  }
}

