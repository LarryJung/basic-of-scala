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

  // https://medium.com/@frank.tan/fibonacci-tail-recursive-explained-876edf5e86fc
  def getFibonacci(index: Int): Int = {
    @tailrec
    def getTailRec(index: Int, prev: Int, current: Int): Int =
      if (index <= 0)
        current
      else
        getTailRec(index - 1, prev = prev + current, current = prev)

    getTailRec(index, prev = 1, current = 0)
  }

  private def formatResult(name: String, n: Int, f: Int => Int): String = {
    val msg = "The %s value of %d is %d"
    msg.format(name, n, f(n))
  }

  // non-generical
  def findFirst_non_generial(ss: Array[String], key: String): Int = {
    @tailrec
    def loop(n: Int): Int =
      if (n >= ss.length) -1
      else if (ss(n) == key) n
      else loop(n + 1)

    loop(0)
  }

  // generic function
  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)

    loop(0)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean =
      if (!ordered(as(n), as(n + 1))) false
      else if (n + 1 == (as.length - 1)) true
      else loop(n + 1)

    loop(0)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    b => f(a, b)

  //(b: B) => f(a, b)

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  //(a: A) => (b: B) => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b) //???

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))

  //   f compose g
  //    g andThen f

  def main(args: Array[String]): Unit
  = {
    println(formatResult("absolute", -42, abs))
    println(formatResult("factorial", 5, factorial))
    println(formatResult("fibonacci", 9, fib))
  }
}