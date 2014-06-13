import join._
import scala.concurrent.ops._

object ChameneosJoin {
  abstract class Colour
  case object RED extends Colour
  case object YELLOW extends Colour
  case object BLUE extends Colour
  case object FADED extends Colour
  
  val colours = Array[Colour](BLUE, RED, YELLOW)
  
  var start: Long = 0L
  var end: Long = 0L
  
  class Mall(var n: Int, numChameneos: Int) extends Join {
    var waitingChameneo:Option[Chameneo] = None

    var sumMeetings = 0
    var numFaded = 0
    
    def startChameneos(): Unit = {
      var i = 0
      while(i < numChameneos) {
        val c = Chameneo(this, colours(i%3), i)
        spawn { c.startChameneo() }
        i = i + 1
      }
    }
    
    object MeetingCount extends AsyName[Int]
    object Meet extends AsyName[(Chameneo, Colour)]
    
    join {
      case MeetingCount(i) => {
//        println(i)
        numFaded = numFaded + 1
        sumMeetings = sumMeetings + i
        if(numFaded == numChameneos) {
          ChameneosJoin.end = System.currentTimeMillis
        }
      }
//      case msg@Meet(c) => {
      case Meet(caller, c) => {
        //println(caller)
        if(n > 0) {
	      waitingChameneo match {
                case Some(chameneo) =>
                  n = n-1
                  chameneo.Meet(caller, c)
                  waitingChameneo = None
                case None =>
                  waitingChameneo = Some(caller)
              }
        } else {
          waitingChameneo match {
            case Some(chameneo) =>
              chameneo.Exit(this, "normal")
            case None => 
          }
          caller.Exit(this, "normal")
        }
      }
    }
  }
  
  case class Chameneo(var mall: Mall, var colour: Colour, id:Int) extends Join {
    var meetings = 0
    
    def startChameneo(): Unit = {
      //println(this )
      mall.Meet(this,colour)
    }
    object Meet extends AsyName[(Chameneo, Colour)]
    object Change extends AsyName[Colour]
    object Exit extends AsyName[(Mall, String)]
    
    join{
      case Meet(caller,otherColour) =>
        colour = complement(otherColour)
        meetings = meetings +1
        caller.Change(colour)
        mall.Meet(this,colour)
      case Change(newColour) =>
        colour = newColour
        meetings = meetings +1
        mall.Meet(this,colour)
      case Exit(mall,_) =>
	    colour = FADED
        mall.MeetingCount(meetings)
    }
    
    def complement(otherColour:Colour): Colour = {
      colour match {
      case RED => otherColour match {
        case RED => RED
        case YELLOW => BLUE
        case BLUE => YELLOW
        case FADED => FADED
      }
      case YELLOW => otherColour match {
        case RED => BLUE
        case YELLOW => YELLOW
        case BLUE => RED
        case FADED => FADED
      }
      case BLUE => otherColour match {
        case RED => YELLOW
        case YELLOW => RED
        case BLUE => BLUE
        case FADED => FADED
      }
      case FADED => FADED
      }
    }
    override def toString() = id+"("+colour+")"
  }
  
  def main(args : Array[String]) : Unit = {
//    val N = 1000000//Integer.parseInt(args(0))
    val N = 1000
    var numChameneos = 4
    if(args.length == 2)
      numChameneos = Integer.parseInt(args(1))
    ChameneosJoin.start = System.currentTimeMillis
    val mall = new Mall(N, numChameneos)
    mall.startChameneos()
    Thread.sleep(3000)
    println("Elapsed: " + (end - start))
  }
}
// val N = 1    Elapsed: 81
// val N = 100  Elapsed: 84