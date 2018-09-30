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

object Main extends App {
//  import PrintDefaults._
  val hello = "hello"
  Print.print(hello)
}