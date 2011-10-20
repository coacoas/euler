package net.coacoas.euler

import java.util.Calendar

object Support { 
  def ??? = throw new NotDefinedError("The method has not been defined")
}

abstract class Problem[T] {
  def run: T
}

abstract class ActiveProblem[T] extends Problem[T] { 
  run
}

trait Timing[T] extends Problem[T] {
  def time(f: () => T): T = {
    val start = Calendar.getInstance.getTimeInMillis()
    val s = f()
    val end = Calendar.getInstance.getTimeInMillis()
    println("Took %d ms".format(end - start))
    s
  }

  abstract override def run: T = time (super.run _) 
}

trait Logging[T] extends Problem[T] {
  abstract override def run: T = {
    lazy val result = super.run
    println(result)
    result
  }
}

