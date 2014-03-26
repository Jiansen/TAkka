package scalability.akka.nqueens

/*
 * A benchmark that finds out all solutions of a N-Queens problem,
 * using backtrack algorithm (recursive)
 * 
 */

import util.{BenchTimer, BenchCounter}

// an n X n board, with queens placed at top rows
// Board(List(1,3,5, 2), 5) =>
//   Q, E, E, E, E
//   E, E, Q, E, E
//   E, E, E, E, Q
//   E, Q, E, E, E
//   E, E, E, E, E


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

  
object NQueenBench extends App{
  val empty = Board(Nil, 13)

//  val board = Board(List(1), 4)
//  board.valid(3) match {
//          case None =>
//          case Some(b) =>
//            b.printboard
//            val ss = NQueen.solutions(b)
//  }
  
  val timer = new BenchTimer()
  timer.start
//  val solutions = NQueen.depth_first_solutions(empty)
  val solutions = NQueen.width_first_solution(List(empty))
//  val solutions = NQueen.width_depth_solution(List(empty), 64)  
  
  println(solutions.size)
  timer.finish
  timer.report
  
//  private val processes:Int = 6000
//  private val messagess:Int = 2000
//
//  private val system = ActorSystem("BangSystem")
//  val testActor = system.actorOf(Props[Bang], "BangBenchActor")
//  testActor ! BangBench(processes,messagess)
}


/*
 *   n		solutions	time (ms)	time (ms)					time (ms)					
 *   8		92			125			122							125					
 *   9		352			158			166							352
 *  10		724			222			217							724
 *  11		2,680		377			426							375
 *  12		14,200		1273		2632						1297
 *  13		73,712		6658		28342						6604
 *  14		365,596		49413		GC overhead limit exceeded	
 *  15
 * 
 */
