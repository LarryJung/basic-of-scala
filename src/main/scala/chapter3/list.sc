import List._

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("tail of empty list")
      case Cons(_, t) => t
    }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("setHead on empty list")
    case Cons(_, t) => Cons(h, t)
  }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x, y) => x + y)

  def sum3(ns: List[Int]): Int =
    foldLeft(ns, 0)(_ + _)

  //  List(1, 2, 3, 4)
  //  foldLeft(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))), 0)(_ + _)
  //  foldLeft(Cons(2, Cons(3, Cons(4, Nil))), 0 + 1)(_ + _)
  //  foldLeft(Cons(3, Cons(4, Nil)), 0 + 1 + 2)(_ + _)
  //  foldLeft(Cons(4, Nil)), 0 + 1 + 2 + 3)(_ + _)
  //  foldLeft(Nil), 0 + 1 + 2 + 3 + 4)(_ + _)
  //  0 + 1 + 2 + 3 + 4

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(x, xs) => x * product(xs)
  }

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _)

  def product3(ns: List[Double]): Double =
    foldLeft(ns, 1.0)(_ * _)

  def addOne(as: List[Int]): List[Int] =
    foldRight(as, List[Int]())((h, t) => Cons(h + 1, t))

  def doubleToString(as: List[Double]): List[String] =
    foldRight(as, List[String]())((h, t) => Cons(h.toString, t))

  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, List[B]())((h, t) => Cons(f(h), t))

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, List[B]())((h, t) => append(f(h), t))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, List[A]())((h, t) => if (f(h)) Cons(h, t) else t)

  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def append2[A](a1: List[A], a2: List[A]): List[A] =
  //    foldRight(a1, a2)((h:A, t:List[A]) => Cons(h, t))
    foldRight(a1, a2)(Cons(_, _))

  def append3[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(reverse(a1), a2)((acc, h) => Cons(h, acc))

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
  }

  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  def connect[A](as: List[List[A]]): List[A] =
    foldRight(as, List[A]())((h, t) => append2(h, t))

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((acc, h) => Cons(h, acc))

  //  List(1, 2, 3, 4)
  //  foldLeft(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))), List[Int]())(acc, h) => Cons(h, acc))
  //  foldLeft(Cons(2, Cons(3, Cons(4, Nil))), Cons(1, List[Int]())(acc, h) => Cons(h, acc))
  //  foldLeft(Cons(3, Cons(4, Nil)), Cons(2, Cons(1, List[Int]()))(acc, h) => Cons(h, acc))
  //  foldLeft(Cons(4, Nil)), Cons(3, Cons(2, Cons(1, List[Int]())))(acc, h) => Cons(h, acc))
  //  foldLeft(Nil, Cons(4, Cons(3, Cons(2, Cons(1, List[Int]()))))(acc, h) => Cons(h, acc))
  //  Cons(4, Cons(3, Cons(2, Cons(1, List[Int]())))

  def length2[A](ns: List[A]): Int =
    foldLeft(ns, 0)((acc, _) => 1 + acc)

  def length[A](ns: List[A]): Int =
    foldRight(ns, 0)((_, acc) => 1 + acc)

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile(t)(f)
      case _ => l
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  def init2[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]

    @tailrec
    def go(cur: List[A]): List[A] = cur match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => List(buf.toList: _*)
      case Cons(h, t) => buf += h; go(t)
    }

    go(l)
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def hasElement[A](l: List[A], x: A): Boolean = l match {
      case Nil => false
      case Cons(h, t) => if (h == x) true else hasElement(t, x)
    }

    sub match {
      case Nil => true
      case Cons(h, t) => if (!hasElement(sup, h)) false else hasSubsequence(sup, t)
    }
  }

  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil) => true
    case (Cons(h, t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }

  // 틀림..
  def init3[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, Cons(t, Nil)) => Cons(h, Nil)
  }
}

List(1, 2, 3, 4, 5) match {
  //  case Cons(x, Cons(2, Cons(4, _))) => x // error
  //  case Nil => 42 // match 오류
  //  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // 3
  //    case Cons(h, t) => h + sum(t) // 15
  case _ => 101 // 101
}

drop(List(1, 2, 3, 4, 5), 4)
List(1, 2, 3, 4, 5)
init(List(1, 2, 3, 4, 5))

var xs: List[Int] = List(1, 2, 3, 4, 5)
val ex1 = dropWhile(xs)(x => x < 4)

product2(List(1.0, 2.0, 3.0))
product3(List(1.0, 2.0, 3.0))
product2(List(1.0, 2.0, 2.0, 2.0, 0.0, 2.0, 3.0))

foldRight(List(1, 3, 4), Nil: List[Int])(Cons(_, _)) //??
length(xs)
length2(xs)

sum3(xs)

reverse(xs)
List[Int]()

append(List(1, 2), List(3, 4))
append2(List(1, 2), List(3, 4))
append3(List(1, 2), List(3, 4))


connect(List(List(1, 2), List(3, 4), List(5, 6)))
addOne(List(1, 2, 3))
doubleToString(List(1.0, 2.0, 3.0))
map(List(1, 2, 3))(x => x.toString)

filter(List(1, 2, 3, 4, 5, 6, 7, 8))(x => x % 2 == 0)
filter2(List(1, 2, 3, 4, 5, 6, 7, 8))(x => x % 2 == 0)

flatMap(List(1, 2, 3))(i => List(i, i))
addPairwise(List(1, 2, 3), List(4, 5, 6))
zipWith(List(1, 2, 3), List("a", "b", "c"))((_, _))

hasSubsequence(List(1, 2, 3, 4), List(1, 3))
startsWith(List(1, 2, 3), List(2, 3))