import join._

object non_linear_pattern extends App{
  object join extends Join{
    object ch extends AsyName[Int]
    object ch2 extends AsyName[Int]
    join{
      case ch(2) and ch(1) => println("Haha")
    }
  }

  join.ch(2)
    join.ch(1)
  println("Hello")
}