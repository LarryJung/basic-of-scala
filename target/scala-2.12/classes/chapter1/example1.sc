// 참조 투명
val x = "Hello, World"
val r1 = x.reverse
val r2 = x.reverse

// 참조 불투명
val x1 = new StringBuilder("Hello")
val y = x1.append(", World")
val r3 = y.toString
val r4 = y.toString
x1.append(", World").toString
x1.append(", World").toString