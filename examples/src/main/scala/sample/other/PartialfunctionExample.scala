package sample.other

sealed trait Fruit
case object Apple extends Fruit
case object Orange extends Fruit

object PartialFunctionExample extends App {  
  val appleNamePF:PartialFunction[Fruit, Unit] = {
    case Apple =>
      println("Apple")
  }
  val orangeNamePF:PartialFunction[Fruit, Unit] = {
    case Orange =>
      println("Orange")
  }
  val fruitnamePF: PartialFunction[Fruit, Unit] = appleNamePF orElse orangeNamePF
  assert(fruitnamePF(Orange).equals("Orange"))
  
  val appleNameF:Fruit=>Unit ={
    case Apple =>
      println("Apple")
  }
//compiler warning: match may not be exhaustive. It would fail on the following input: Orange  
  val orangeNameF:Fruit=>Unit ={
    case Orange =>
      println("Orange")
  }
//compiler warning: match may not be exhaustive. It would fail on the following input:  Apple
  val fruitNameF:Fruit=>Unit ={
    case Apple =>
      appleNameF(Apple)
    case Orange =>
      orangeNameF(Orange)
  }
}
