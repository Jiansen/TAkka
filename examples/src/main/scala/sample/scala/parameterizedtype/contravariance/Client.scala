package sample.scala.parameterizedtype.contravariance

object Client extends App {
  var stack:Stack[Integer] = new ArrayStack[Integer](Nil)
  
  var i = 0
  for(i <- 0 until 4) { stack = stack.push(new Integer(i)) }
//  println(stack)
  assert(stack.toString().equals("stack(3, 2, 1, 0)"))
  stack.pop match {
    case (top, stack) =>
//      println(stack)  
      assert(top == 3 && stack.toString().equals("stack(2, 1, 0)"))
      val reverse:Stack[Integer] = Stacks.reverse(stack)
//  println(reverse)  
      assert(reverse.toString().equals("stack(0, 1, 2)"))
      val anystack:Stack[Any] = reverse.push(3.0)
      assert(anystack.toString().equals("stack(3.0, 0, 1, 2)"))
  }
}