package sample.scala.parameterizedtype.generic

object Client extends App {
  val stack:Stack[Integer] = new ListStack[Integer]
  
  var i = 0
  for(i <- 0 until 4) stack.push(i)
//  println(stack)
  assert(stack.toString().equals("stack(3, 2, 1, 0)"))
  val top = stack.pop
//  println(stack)  
  assert(top == 3 && stack.toString().equals("stack(2, 1, 0)"))
  val reverse = Stacks.reverse(stack)
//  println(stack)
//  println(reverse)  
  assert(stack.empty)
  assert(reverse.toString().equals("stack(0, 1, 2)"))
}