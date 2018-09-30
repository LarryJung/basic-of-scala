package etc.monoidStudy

import cats._
import cats.implicits._

/**
  * 이번엔 모노이드 법칙에 대해서 알아보고 모노이드의 간단한 사용을 알아봤다.
  * 이러한 모노이드 법칙을 만족한다면 우린 combine 연산이 결합법칙을 만족한다는 것을 추론 할 수 있고
  * 목록 같은 자료구조를 fold 할때 병렬로 처리 할 수도 있게다는 것도 추론 할수 있다.
  *
  */

object MonoidExample extends App {
  val empty = Monoid[String].empty
  val list1 = Monoid[String].combineAll(List("a", "b", "c"))
  // same above
  val list2 = Monoid.apply[String].combineAll(List("a", "b", "c"))
  val stringResult = "Hi " + "there" + Monoid[String].empty


  println(empty)
  println(list1)
  println(list2)
  println(stringResult)
}
