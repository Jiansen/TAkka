package sample.scala.generic

object Client extends App {
  val stack:Stack[Integer] = new ArrayStack[Integer]
  
  var i = 0
  for(i <- 0 until 4) stack.push(i)
  
  assert(stack.toString().equals("stackList(0, 1, 2, 3)"))
  val top = stack.pop
  assert(top == 3 && stack.toString().equals("stackList(0, 1, 2)"))
  val reverse = Stacks.reverse(stack)
  assert(stack.empty)
  assert(reverse.toString().equals("stackList(2, 1, 0)"))
}