import join._
import org.scalacheck.Prop.forAll
import org.scalacheck._
import org.scalacheck.Prop._



object gen_msg extends App{
  
  var messages : List[(NameBase, Any)] = List()
  
  object join extends Join{  
    object A extends AsyName[Int]  
    object B extends AsyName[Int]
    object C extends AsyName[Int]
    object D extends AsyName[Int]
    object E extends AsyName[Int]  
    join {
     case A(1) and B(1) =>  {  }
     case C(2) and B(2) and D(2) =>  { }
     case B(3) and D(3) and E(3) =>  { }
     case E(4)=>  { }
    }
    
  }
      
  
  val ch_msg_gen = Gen.oneOf(
       (join.A, 1),
       (join.B, 1), (join.B, 2), (join.B, 3),
                    (join.C, 2),
                    (join.D, 2), (join.D, 3),
                                 (join.E, 3), (join.E, 4)
      )  
  
  val prop = forAll (ch_msg_gen) { ch_msg =>
      ch_msg match {
         case (channel, msg) =>  {
           /*
           channel match {
             case join.A => println("join.A("+msg+")")
             case join.B => println("join.B("+msg+")")
             case join.C => println("join.C("+msg+")")
             case join.D => println("join.D("+msg+")")
             case join.E => println("join.E("+msg+")")
           }
           */
           //spawn {channel(msg)}
           messages = (channel,msg)::messages
      }
      true
    }
  }
  for(i <- List.range(0,100)){
    prop.check
  }
  
  val sout = new java.io.FileWriter("./ScalaMessages.txt")
  var i = 0
  for((channel, msg) <- messages){
    if(i%5 == 0) sout.write("\n")
    channel match {
      case join.A => sout.write("join.A("+msg+"); ")
      case join.B => sout.write("join.B("+msg+"); ")
      case join.C => sout.write("join.C("+msg+"); ")
      case join.D => sout.write("join.D("+msg+"); ")
      case join.E => sout.write("join.E("+msg+"); ")
    }
    i += 1
    if(i%100 == 0) sout.write("// "+i+" messages\n")
  }
  sout.close
  
  val jout = new java.io.FileWriter("./JoinMessages.txt")
  i = 0
  for((channel, msg) <- messages){
    if(i%5 == 0) jout.write("\n")
    channel match {
      case join.A => jout.write("a("+msg+") & ")
      case join.B => jout.write("b("+msg+") & ")
      case join.C => jout.write("c("+msg+") & ")
      case join.D => jout.write("d("+msg+") & ")
      case join.E => jout.write("e("+msg+") & ")
    }
    i += 1
    if(i%100 == 0) jout.write("(* "+i+" messages*)\n")
  }
  jout.close
}