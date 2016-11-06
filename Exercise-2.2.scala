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
