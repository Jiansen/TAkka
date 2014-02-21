package sample.java.parameterizedtype.generic;


//interface Stack<? extends E> { // Syntax error on token "?", invalid TypeParameter
interface Stack<E> {
    public boolean empty();
    public void push(E elt);
    public E pop();
}