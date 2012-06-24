package sample.elevatorController.takka

import org.scalacheck._
import org.scalacheck.Prop._

object ElevatorPlainEQC {
  config.eqctimeout = 1
  sys_event.addHandler(handler_eqc)
  
  case class EQCTrace(var fbs:List[Floor], var elevators:List[(Int, List[Floor])])
  
  sealed trait COMMAND
  private case class waitCom(timeout:Int) extends COMMAND
  private case class fButtonCom(f:Int) extends COMMAND
  private case class eButtonCom(e:Int, f:Int) extends COMMAND

  
  private var count:Int = 0
/*
  case class prop_stop_where_requested_single_command(floors:Int,elevators:Int) extends Commands{
    case class State(n:Int)
    
    def initialState() = {
      util.start_sup(1, floors, elevators)
      sys_event.addHandler(handler_eqc.init((1, floors, elevators)))
      Thread.sleep(1000)
      State(0)
    }
    
    object wait50 extends Command {
      def run(s: State) = { wait(50) }
      def nextState(s: State) = State(s.n + 1)
    }
    
    case class fButtonCom(f:Int) extends Command {
      def run(s: State) = { scheduler.f_button_pressed(Floor(f)) }
      def nextState(s: State) = State(s.n + 1)
      postConditions += {
        case _ => 
          val state = inittrace(elevators)
          val passed = valid_opened(getevents(), state)
          if(!passed) {
            println(getevents())
            println()
            println(state)
            println()
            println()
          }
          passed
      }
    }
    
    case class eButtonCom(e:Int, f:Int) extends Command {
      def run(s: State) = { scheduler.e_button_pressed(e, Floor(f)) }
      def nextState(s: State) = State(s.n + 1)
      postConditions += {
        case _ => 
          val state = inittrace(elevators)
          val passed = valid_opened(getevents(), state)
          if(!passed) {
            println(getevents())
            println()
            println(state)
            println()
            println()
          }
          passed
      }
    }
    
    def genCommand(s: State): Gen[Command] = {
      for {
        command <- Gen.choose(2,3)
        e <- Gen.choose(1, elevators)
        f <- Gen.choose(1, floors)
      } yield command match {
        case 1 => wait50
        case 2 => fButtonCom(f)
        case 3 => eButtonCom(e, f)
      }
    }
  }
  
  */
  def prop_stop_where_requested_single(floors:Int,elevators:Int) = {
    val commandsGen = commands(floors,elevators)
    val superActor = util.start_sup(1, floors, elevators)
    Thread.sleep(1000)
    val eqc_handler = handler_eqc.init((1, floors, elevators))
    //sys_event.addHandler(eqc_handler)    
    val test = forAll(commandsGen) { coms =>       
      wait(10)
      wait(50)
      evelc(coms)
      wait(7*floors*elevators)
      true
    }
    test.check
    // check { (a: Int, b: Int) => 
    //   (a + b) must_== (b + a) 
    // }.set(minTestsOk -> 200, workers -> 3) 
    
    util.stop_sup(superActor)
    //sys_event.removeHandler(eqc_handler)
    
    val trace = collectevents()     
    if(!valid_opened(trace, inittrace(elevators))) {
      println("Test Failed")
      println(trace)
      println(trace.size)
      false
    }else{
      true
    }
  }
  
  val feG = for {
     f <- Gen.choose(2,10)
     e <- Gen.choose(1,4)
  } yield (f, e)

  val prop_stop_where_requested = Prop.forAll(feG) { fe => fe match {
      case (floors, elevators) => 
        println("Testing " + elevators +" elevators between " + floors + " floors.")
        prop_stop_where_requested_single(floors:Int,elevators:Int)
  }}
  
  private def eval(coms:List[Product with Serializable with ElevatorPlainEQC.COMMAND]):Unit = coms match {
    case Nil => Unit
    case com::coms => evelc(com); wait(500); eval(coms)    
  }
  
  private def evelc(com:Product with Serializable with ElevatorPlainEQC.COMMAND):Unit = com match {
    case waitCom(50) => wait(50)
    case fButtonCom(f) => scheduler.f_button_pressed(Floor(f))
    case eButtonCom(e, f) => scheduler.e_button_pressed(e, Floor(f))
  }
  // generators
  // hard to ensure that I always add something to begin or end or 
  // sequence. Only possibility is to have state side effect
  def commands(floors:Int,elevators:Int) : Gen[Product with Serializable with COMMAND] = {
    val com = for {
      command <- Gen.choose(2,3)
      e <- Gen.choose(1, elevators)
      f <- Gen.choose(1, floors)
    } yield command match {
        case 1 => waitCom(50)
        case 2 => fButtonCom(f)
        case 3 => eButtonCom(e, f)
    }
    com
  }
  
  
  // Boolean properties
  def valid_opened(es:List[EQCEvent], state:EQCTrace):Boolean = es match {
    case Nil => true
    case OpenEQCEvent(elev, floor)::es =>
      check_open(elev,floor,state) && valid_opened(es,rm_open(elev,floor,state))
    case FloorEQCEvent(floor) ::es => 
      valid_opened(es, add_fb(floor, state))
    case ElevatorEQCEvent(eNo,floor) :: es =>
      valid_opened(es, add_elev(eNo,floor,state))
  }
  
// elevator E is opened at floor F, check whether
// there is a corresponding goal
  def check_open(e:Int,f:Floor,state:EQCTrace):Boolean = {
    state.elevators(e-1) match {
      case (eNo, stoplist) =>
        val res = state.fbs.contains(f)  || stoplist.contains(f)
        if(!res){
          println("Test Fails when elevator "+e+" is opened at "+f)        
          println("state.fbs: "+state.fbs)
          println("stoplist: "+stoplist)
        }
        res
    }
  }
  
  def inittrace(elevators:Int):EQCTrace = {
    val elevs = for (i<- 1 to elevators) yield (i, Nil)
    EQCTrace(Nil, elevs.toList)
  }
    
  def add_fb(f:Floor,state:EQCTrace):EQCTrace = {
    state.fbs = state.fbs++List(f)
    state
  }

  def add_elev(e:Int, f:Floor, state:EQCTrace):EQCTrace = {
    state.elevators = 
      for (isl <- state.elevators) yield {
        if (e == isl._1) (isl._1, isl._2++List(f))
        else isl
      }
    state
  }

  def rm_open(elev:Int, floor:Floor, state:EQCTrace):EQCTrace = {
    state.fbs = rm_floor(floor, state.fbs)
    state.elevators = 
      for (isl <- state.elevators) yield {
        if (elev == isl._1) (elev, rm_floor(floor, isl._2))
        else isl
      }
    state
  }

  def rm_floor(floor:Floor, floors:List[Floor]):List[Floor] = {
    floors.filter(f=> f != floor)
  }
  
  // commands
  def wait(msec:Int) = {
    Thread.sleep(msec)
  }

  def collectevents():List[EQCEvent] = {
    wait(90)
    handler_eqc.opened()
  }
  
  def getevents():List[EQCEvent] = {
    wait(90)
    handler_eqc.getevents()
  }
}