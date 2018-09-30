package etc

import cats._
import etc.CatsShow.Person

object CatsShow extends App {

  case class Person(name: String, age: Int)
  val showPerson = Show.apply[Person]

  object Person {

    implicit val persionPrintable : Show[Person] = Show.show[Person] {
      p => s"${p.name} is ${p.age} year old"
    }
  }

  implicit class ShowSyntax[T](value: T) {
    def show(implicit s: Show[T]): String = s.show(value)
  }
  println(Person("aaa", 10).show)
  println(showPerson.show(Person("aaa", 10)))
}
