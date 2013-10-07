package sample.java.parameterizedtype.generic;

class Client {
    public static void main(String[] args) {
      Stack<Integer> stack = new ArrayStack<Integer>();
      for (int i = 0; i<4; i++) stack.push(i);
      assert stack.toString().equals("stack[0, 1, 2, 3]");
      int top = stack.pop();
      assert top == 3 && stack.toString().equals("stack[0, 1, 2]");
      Stack<Integer> reverse = Stacks.reverse(stack);
      assert stack.empty();
      assert reverse.toString().equals("stack[2, 1, 0]");
    }
}