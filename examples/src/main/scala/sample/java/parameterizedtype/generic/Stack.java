package sample.java.parameterizedtype.generic;

interface Stack<E> {
    public boolean empty();
    public void push(E elt);
    public E pop();
}