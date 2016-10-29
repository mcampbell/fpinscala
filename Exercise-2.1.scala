object Exercise21 {

  def fib(n : Int): Int = {
    @annotation.tailrec
    def go(n: Int, a: Int, b: Int):Int = {
      if (n == 0)
        b
      else
        go(n - 1, b, a + b)
    }

    if (n == 0) // this feels wrong. How to get the initial 0?
      0
    else
      go(n, 0, 1)
  }

  def main(args: Array[String]) = {
    println(fib(0))
    println(fib(1))
    println(fib(2))
    println(fib(3))
    println(fib(4))
    println(fib(5))
    println(fib(6))
    println(fib(7))
    println(fib(8))
    println(fib(9))
    println(fib(10))
  }
}
