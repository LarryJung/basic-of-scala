import scala.annotation.tailrec

object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    @tailrec
    def loop(n: Int, acc: Int): Int =
      if (n <= 0) acc else loop(n - 1, n * acc)

    loop(n, 1)
  }

  def fib(n: Int): Int = {
    @tailrec
    def loop(n: Int, present: Int, first: Int, second: Int): Int = {
      if (present == n) first + second
      else if (present == 1)
        loop(n, present + 1, 0, 1)
      else if (present == 2)
        loop(n, present + 1, 1, 1)
      else loop(n, present + 1, first + second, first)
    }

    loop(n, 0, 1, 0)
  }

  private def formatAbs(x: Int) = {
    val msg = "The absoulte value of %d is %d"
    msg.format(x, abs(x))
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(factorial(5))
    println(fib(9))
  }
}