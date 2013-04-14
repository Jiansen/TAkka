package sample.other

import java.io._
import scala.reflect.runtime.universe._
 
object serializationExample extends App {
    class Foo(m: String) extends scala.Serializable {
        val message = m
    }
 
//    val foo = new Foo("qweqwe")
    val foo = typeTag[Foo]  

    val output = new ObjectOutputStream(new FileOutputStream("test.obj"))
    output.writeObject(foo)
    output.close
 
    val input = new ObjectInputStream(new FileInputStream("test.obj"))
    val bar = input.readObject()
    input.close()
 
    println(bar)
}