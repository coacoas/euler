package net.coacoas.euler

import java.util.Calendar

abstract class GenericProblem[T] extends App {
  def ??? = throw new NotDefinedError("The method has not been defined")
  def run: T
  def _run = run
  println(_run)
}

trait Timing[T] extends GenericProblem[T] {
  def time(f: () => T): T = {
    val start = Calendar.getInstance.getTimeInMillis()
    val s = f()
    val end = Calendar.getInstance.getTimeInMillis()
    println("Took %d ms".format(end - start))
    s
  }

  abstract override def _run: T = time (super._run _) 
}

abstract class Problem extends GenericProblem[Long] with Timing[Long]