package sample.scala.parameterizedtype.typebound

object Client extends App {
  val stack:Stack[Integer] = new ArrayStack[Integer]
  
  var i = 0
  for(i <- 0 until 4) stack.push(new Integer(i))
//  println(stack)
  assert(stack.toString().equals("stack(0, 1, 2, 3)"))
  val top = stack.pop
//  println(stack)  
  assert(top == 3 && stack.toString().equals("stack(0, 1, 2)"))
  val reverse = Stacks.reverse(stack)
//  println(stack)
//  println(reverse)  
  assert(stack.empty)
  assert(reverse.toString().equals("stack(2, 1, 0)"))
}