class Cafe {
  def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
    val cup = Coffee(5000)
    (cup, Charge(cc, cup.price))
  }

  def buyCoffees(cc: CreditCard, n: Int): (List[Coffee], Charge) = {
    val purchases: List[(Coffee, Charge)] = List.fill(n)(buyCoffee(cc))
    val (coffees, charges) = purchases.unzip
    (coffees, charges.reduce((c1, c2) => c1.combine(c2)))
  }
}

case class CreditCard(value: String) {
  def charge(price: Int): Unit = Unit
}

case class Coffee(price: Int)

case class Charge(cc: CreditCard, amount: Int) {
  def combine(other: Charge): Charge =
    if (cc == other.cc)
      Charge(cc, amount + other.amount)
    else
      throw new RuntimeException("Can't combine charges to different cards")
}

