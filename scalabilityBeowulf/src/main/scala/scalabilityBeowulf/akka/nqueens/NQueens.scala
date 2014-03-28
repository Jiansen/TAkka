package scalabilityBeowulf.akka.nqueens

/*
 * A benchmark that finds out all solutions of a N-Queens problem,
 * using backtrack algorithm (recursive)
 * 
 */

import util.{BenchTimer, BenchCounter}
import akka.actor.ActorSystem
import scalabilityBeowulf.BeowulfConfig._
import akka.actor.Props
import akka.actor.Actor
import akka.actor.ActorRef


// an n X n board, with queens placed at top rows
// Board(List(1,3,5, 2), 5) =>
//   Q, E, E, E, E
//   E, E, Q, E, E
//   E, E, E, E, Q
//   E, Q, E, E, E
//   E, E, E, E, E


case class NQueenMasterStart(n:Int, nodes:Int)
case class Search(board:Board, master:ActorRef)
case class Solutions(boards:List[Board])

case class Board(val queens:List[Int], val n:Int){
  
  /*
   *  can we put a Queen at (queens.size()+1,k)?
   */
  def valid(k:Int): Option[Board] = {

    var r = 1;
    for(q <- queens){
      // check row, always OK          
      // check col
      if(q == k) {return None}
      // check diagonal
      val diff_row = queens.size - r+1;
      if(q == k-diff_row || q == k+diff_row) {return None}
      
      r+=1;
    }
    return Some(Board(queens :+ (k), n))
  }
  
  def printboard(){
    for (q <- queens){
      for (i <- 1 to n){
         if (i == q) { print("Q ") }
         else{ print("E ") }
      }
      println()
    }
    for (j <- queens.size+1 to n){
      for (i <- 1 to n){
         print("E ")
      }
      println()
    }    
    println("------")
    println("------")    
  }
}



object NQueen {  
  
  // width first search
  def width_first_one_step(boards:List[Board]):List[Board] = {
    var solutions:List[Board] = Nil;
    for (board <- boards){
//      board.printboard
      for(i <- 1 to board.n){
        board.valid(i) match {
          case None =>
          case Some(b) =>
//            b.printboard
            solutions = b :: solutions
        }
      }
    }
    return solutions;
  }
  
  def width_first_solution(boards:List[Board]) : List[Board] = {
    if (boards.size == 0) return boards
    if(boards.head.queens.size == boards.head.n) { return boards}
    
//      println(boards.size)    
    return width_first_solution(width_first_one_step(boards))
  }
  
  //  deepth-first search  
  def depth_first_solutions(board:Board):List[Board] = {
    if(board.queens.size < board.n){
      var solutions:List[Board] = Nil;
      for(i <- 1 to board.n){
        board.valid(i) match {
          case None =>
          case Some(b) =>
//            b.printboard
            val ss = NQueen.depth_first_solutions(b)
            solutions = solutions ::: ss
        }
      }
      return solutions
    }else{
      return List(board)      
    }
  }
  
  
  
  def width_depth_solution(boards:List[Board], threadhold:Int) : List[Board] = {
    if (boards.size == 0) return boards
    if(boards.head.queens.size == boards.head.n) { return boards}
    
    if(boards.size < threadhold){
//      val bs = width_first_one_step(boards);
//      println(boards.size)
      width_depth_solution(width_first_one_step(boards), threadhold)
    }else{
      var solutions:List[Board] = Nil;
      for(board <- boards){
        solutions =  depth_first_solutions(board) ::: solutions
      }
      return solutions      
    }
  }
}

class NQueenDFS extends Actor {
  def receive = {
    case Search(board:Board, master:ActorRef) => {
      master ! Solutions(NQueen.depth_first_solutions(board))
    }
  }
  
}

class NQueenMaster extends Actor {
  val timer = new BenchTimer
  
  var n_partial_solutions = 0
  var solutions:List[Board] = Nil
  
  
  def receive = {
    case NQueenMasterStart(n, nodes) =>
      timer.start      
      // width-first search to expand the search space
      val empty = Board(Nil, n)      
      val partial_solutions = width_expand(List(empty), nodes * 2)
      
      // no need for depth first search
      if (partial_solutions.size == 0) complete( partial_solutions )
      if(partial_solutions.head.queens.size == partial_solutions.head.n) { complete( partial_solutions)}

      
      // delegate to child processes
      val plist = (for (i<- 1 to nodes) yield {
        context.actorOf(Props[NQueenDFS], ProcessNamePrefix+i)
      })
      
      this.n_partial_solutions = partial_solutions.size 
      var i = 0;
      for (board <- partial_solutions){
        val p = plist.apply(i % nodes)
        p ! Search(board, self)
        i += 1
      }
    case Solutions(ss) =>
      this.solutions = this.solutions ::: ss
      this.n_partial_solutions -= 1
      if (this.n_partial_solutions == 0) {
        complete(solutions)
      }
  }
  
  def complete(solutions:List[Board]) = {
    timer.finish
    timer.report
    
    println("\n "+solutions.size+" solutions found.\n");
    
    sys.exit
  }
  
  def width_expand(boards:List[Board], threadhold:Int) : List[Board] = {
    if (boards.size == 0) return boards
    if(boards.head.queens.size == boards.head.n) { return boards}
    
    if(boards.size < threadhold){
      return width_expand(NQueen.width_first_one_step(boards), threadhold)
    }else{
      return boards;
    }
  }
}
  
object NQueenBench extends App{
  // compute n_queens problem with 'nodes' nodes
  private val n_queens:Int = args(0).toInt  
  private val nodes:Int = args(1).toInt

  private val system = ActorSystem("NQueenSystem", masterNodeConfig(WorkerNodePrefix, ProcessPathPrefix, ProcessNamePrefix, nodes, nodes))
  val testActor = system.actorOf(Props[NQueenMaster], ProcessPathPrefix)
  testActor ! NQueenMasterStart(n_queens, nodes)
}

