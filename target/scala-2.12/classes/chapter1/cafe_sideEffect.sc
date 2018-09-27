class Cafe {
  def buyCoffee(cc: CreditCard): Coffee = {
    val cup = Coffee(5000)
    cc.charge(cup.price) // 부수적 효과가 있다.
    cup
  }
}

case class CreditCard(value: String) {
  def charge(price: Int): Unit = Unit
}

case class Coffee(price: Int)