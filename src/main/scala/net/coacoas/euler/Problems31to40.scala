package net.coacoas.euler

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