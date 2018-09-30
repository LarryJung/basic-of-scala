val myList = List("England" -> "London", "Germany" -> "Berlin")
val myMap = myList.groupBy(e => e._1).map(e => (e._1, e._2(0)._2))
val betterConversion = Map(myList: _*)
val scala28Map = myList.toMap

List(List(1, 2, 3), List(4, 5, 6)).flatMap(_)

def toInt(s: String): Option[Int] = {
  try {
    Some(Integer.parseInt(s.trim))
  } catch {
    case e: Exception => None
  }
}

val strings = Seq("1", "2", "foo", "3", "bar")

strings.map(toInt) // List(Some(1), Some(2), None, Some(3), None)
strings.flatMap(toInt) // List(1, 2, 3)


