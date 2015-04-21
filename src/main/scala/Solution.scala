import cmpsci220.hw.tictactoe._
import scala.util.control._
import scala.Boolean


class Game(val turn: Player, val board: Matrix[Option[Player]]) extends GameLike[Game] {
// class Game(/* add fields here */) extends GameLike[Game] {


  /*
  Implement the isFinished method. Human players often end a game early, when the outcome is inevitable.
  However, you may find it easier to write a program that plays until every single square is filled. 
  Remember that if you determine that the game isFinished, you need to be able to determine the winner too (using getWinner).
  */
  def isFinished(): Boolean = { 
    var isPlayed = false
    var count:Int = 0
    
    isSequenceGood(board.antiDiagonal) // iff XXX || OOO 
    isSequenceGood(board.mainDiagonal) // iff XXX || OOO
    for(s<-board.cols){ isSequenceGood(s) } // iff XXX || OOO
    for(s<-board.rows){ isSequenceGood(s) } // iff XXX || OOO
    if(isPlayed){ return isPlayed }
    
    for(s<-board.cols){ for(t<-s){ if(t == None){ isPlayed = false}  } }//iff None exists in nxn  
    for(s<-board.rows){ val iter = s.iterator; while(iter.hasNext){ val ctrl = iter.next; if(ctrl == Some(X) || ctrl == Some(O)) { count += 1}  } } // check for draws
    if(count == board.rows.length*board.rows.length) {isPlayed = true}  // if nxn == 3x3, then draw will occur i.e. if count == 9

    def isSequenceGood(lst:scala.List[Option[Player]]):Unit = { 
      val x = board.rows.length 
      val someListX = Matrix[Option[Player]](x, Some(X)).rows.head
      val someListO = Matrix[Option[Player]](x, Some(O)).rows.head
      if(lst.containsSlice( someListX ) || 
      lst.containsSlice( someListO )) { isPlayed = true } }  // iff XXX || OOO

    isPlayed
  }
  

  /* Assume that isFinished} is true */
  def getWinner(): Option[Player] = {
    var winner:Option[Player] = None
    var count:Int = 0; var foundX:Boolean = false; var foundO:Boolean = false 

    isSequenceGood(board.antiDiagonal) // iff XXX || OOO
    isSequenceGood(board.mainDiagonal) // iff XXX || OOO
    for(s<-board.cols){ isSequenceGood(s) } // iff XXX || OOO
    for(s<-board.rows){ isSequenceGood(s) } // iff XXX || OOO 
    if(foundO && foundX){ return None} // if more than one winner, it is DRAW
    if(winner == Some(X) || winner == Some(O)){ return winner }
    

    for(s<-board.rows){ val iter = s.iterator; while(iter.hasNext){ val ctrl = iter.next; if(ctrl == Some(X) || ctrl == Some(O)) { count += 1}  } } // check for draws
    if(count == board.rows.length*board.rows.length) {winner = None}  // if nxn == 3x3, then draw will occur i.e. if count == 9

    def isSequenceGood(lst:scala.List[Option[Player]]):Unit = { 
      val x = board.rows.length 
      val someListX = Matrix[Option[Player]](x, Some(X)).rows.head
      val someListO = Matrix[Option[Player]](x, Some(O)).rows.head
      if(lst.containsSlice( someListX )) {foundX=true; winner = Some(X)} // iff XXX || OOO
      else if (lst.containsSlice( someListO )) {foundO=true; winner = Some(O) } }  // iff XXX || OOO
    
    winner
 }



  def nextBoards(): List[Game] = {
    var i:Int = 0 
    var nextPlayer:Player = turn
    var stewie:List[Game] = List[Game]()
    var arrNones:List[(Int, Int)] = List[(Int, Int)] () 
    if(turn == O) nextPlayer = X else nextPlayer = O


    for(s<-board.cols){ for(j<-0 to s.length-1){ if( board.get(i, j) == None){ stewie = stewie:+new Game(nextPlayer, board.set(i, j, Some(turn)))  }  }; i+=1 }; i=0 //iff None exists in nxn, write its i, j indices into a new board object of a new Game object and set Game object into a list until condition fails 
    println("initial board is below: ")
    
    for(s<-board.rows){ for(t<-s){ if(!t.isEmpty) { printf("%s\t", t.get)} else printf(s"$t\t") }; println() }; println() 
    for(s<-stewie) { for(a<-s.board.rows){for(t<-a){ if(!t.isEmpty) { printf("%s\t", t.get) } else {printf(s"$t\t")} }; println("i is: " +i); if(i> a.length-2){ i = 0; printf("The Next Player is: %s\n\n", nextPlayer); } else {i+=1}}} // it works!
    
    stewie: 
    List[Game]
  }
}

object Solution extends MinimaxLike { // 

  type T = Game 

  def createGame(turn: Player, board: Matrix[Option[Player]]): Game = {
    require(board.dim == 3)
    new Game(turn, board) 
  } 

/**
  If it is Xs turn:

    1. If game is a winning state for X, return Some(X)
    2. If game is a draw state, return None
    3. Recursively apply minimax to all the successor states of game
       - If any recursive call produces X, return Some(X)
       - Or, if any recursive call produces None, return None
       - Or, return Some(O)

  The case for Os turn is similar.
**/
  def minimax(board: Game): Option[Player] = {
    var aPlayer:Option[Player] = None
    var count:Int = 0

    for(s<-board.board.rows){ val iter = s.iterator; while(iter.hasNext){ val ctrl = iter.next; if(ctrl == None) { count += 1}  } } //empty board should return None
    if(count == board.board.rows.length*board.board.rows.length) {aPlayer = None; return aPlayer}  // if nxn == 3x3 Nones, then empty board will return None

    if(Some(board.turn) == Some(X:Player)){
     returnedPlayer
    } else { returnedPlayer }
    

    def returnedPlayer():Unit = { // funtionally applies recurring to tail. r
      println("printing board in minimax ")
      for(s<-board.board.rows){ for(t<-s){ if(!t.isEmpty) { printf("%s\t", t.get)} else printf(s"$t\t") }; println() }; println() 
      if(board.isFinished){
        aPlayer = board.getWinner match {
          case Some(s) => Some(s)
          case None => None
      }

      } else { // if Some(X ) fails apply to O , recording all instances of Some X and Some O 
        val iter = board.nextBoards.iterator
        while(iter.hasNext){
          aPlayer = minimax(iter.next) match { // 3. apply minimax to all the successor states of game
          case Some(s) => Some(s)
          case None => None
          }
        }
      }
    }

    aPlayer
  }
}

