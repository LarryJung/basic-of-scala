import cats.Functor

object FunctorExample extends App {

  sealed trait Result[+A]

  final case class Success[A](value: A) extends Result[A]

  final case class Warning[A](value: A, message: String) extends Result[A]

  final case class Failure(message: String) extends Result[Nothing]

  object Result {
    implicit val resultFunctor = new Functor[Result] {
      def map[A, B](result: Result[A])(func: A => B): Result[B] =
        result match {
          case Success(value) => Success(func(value))
          case Warning(value, message) => Warning(func(value), message)
          case Failure(message) => Failure(message)
        }
    }

    def success[A](value: A): Result[A] =
      Success(value)

    def warning[A](value: A, message: String): Result[A] =
      Warning(value, message)

    def failure[A](message: String): Result[A] =
      Failure(message)
  }

  import Result._

  success(100) map (_ * 2) // ??

}