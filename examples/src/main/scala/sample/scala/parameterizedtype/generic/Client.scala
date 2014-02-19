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
  
//  var stack2:Stack[Any] = stack    // compile error
  
  //type mismatch; found : sample.scala.parameterizedtype.generic.Stack[Integer] required: sample.scala.parameterizedtype.generic.Stack[Any] 
//  Note: Integer <: Any, but trait Stack is invariant in type E. You may wish to define E as +E instead. (SLS 4.5)
}