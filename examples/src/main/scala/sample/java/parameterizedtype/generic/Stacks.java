package sample.java.parameterizedtype.generic;

class Stacks {
    public static <T> Stack<T> reverse(Stack<T> in) {
      Stack<T> out = new ArrayStack<T>();
      while (!in.empty()) {
        T elt = in.pop();
        out.push(elt);
      }
      return out;
    }
}