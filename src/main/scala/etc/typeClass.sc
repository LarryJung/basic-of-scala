import PrintDefaults._
import PrintSyntax._

trait Printable[A] {
  def format(value: A): String
}

object PrintDefaults {
  implicit val s = new Printable[String] {
    override def format(input: String) = input
  }
  implicit val i = new Printable[Int] {
    override def format(input: Int) = input.toString
  }
}

object Print {
  def format[A](input: A)(implicit printer: Printable[A]): String = {
    printer.format(input)
  }

  def print[A](input: A)(implicit printer: Printable[A]): Unit = {
    println(printer.format(input))
  }
}

val hello = "hello"
//Print.print(hello)

object PrintSyntax {

  implicit class PrintOps[A](value: A) {
    def format(implicit printable: Printable[A]): String = {
      printable.format(value)
    }

    def print(implicit printable: Printable[A]): Unit = {
      println(printable.format(value))
    }
  }

}

"hello".print

case class Person(name: String, age: Int)

object Person {
  implicit val personPrintable = new Printable[Person] {
    override def format(value: Person): String = s"${value.name} is ${value.age} year old"
  }
}

//Print.print(Person("mason", 31))
Person("mason", 30).print