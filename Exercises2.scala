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


////////////////////////////////////////////////////////////////////////////////
object Exercise22 {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

    @annotation.tailrec
    def loop(idx: Int): Boolean = {
      if (idx >= as.length - 1)
        true
      else if (!ordered(as(idx), as(idx + 1)))
        false
      else
        loop(idx + 1)
    }

    loop(0)
  }

  private def intSmaller(a: Int, b: Int) =
    a.compareTo(b) < 0

  private def stringSmaller(a: String, b: String) =
    a.compareTo(b) < 0

  def main(args: Array[String]) = {
    println(isSorted(Array(3, 1, 2, 3), intSmaller))
    println(isSorted(Array(1, 2, 3), intSmaller))
    println(isSorted(Array("foo", "bar", "baz"), stringSmaller))
    println(isSorted(Array("bar", "baz", "foo"), stringSmaller))
  }
}

////////////////////////////////////////////////////////////////////////////////
object Exercise23 {

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (
      (b: B) => f(a, b)
    )
}
