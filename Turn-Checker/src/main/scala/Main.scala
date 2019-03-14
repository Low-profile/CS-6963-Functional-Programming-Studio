
case class Player(tokens: Tokens, card: String) {
  def moveTokenTo(tokIdx: Int, to: Pos) = {
    if (tokIdx == 1)
      Player(new Tokens(to, tokens.tok2), card)
    else
      Player(new Tokens(tokens.tok1, to), card)

  }
}

case class Pos(val row: Int, val col: Int) {
  override def toString: String = "(" + row + ", " + col + ")"

  override def equals(obj: Any): Boolean = {
    obj match {
      case x: Pos => x.row == row && x.col == col
      case _ => super.equals(obj)
    }
  }

  def -(that: Pos): Pos = {
    Pos(row - that.row, col - that.col)
  }

  def +(that: Pos): Pos = {
    Pos(that.row + row, that.col + col)
  }

  def outOfBound(): Boolean = {
    row <= 0 || row > 5 || col <= 0 || col > 5
  }

  def toList() = {
    List(row, col)
  }

  def getSurroundings(): List[Pos] = {
    val directions = List(-1, 0, 1)
      .flatMap(x => List(-1, 0, 1).map(y => List(x, y)))
      .filterNot(_ == List(0, 0))

    directions.map(x => Pos(row + x(0), col + x(1)))
  }
}

class Tokens(val tok1: Pos, val tok2: Pos) {

  override def equals(obj: Any): Boolean = {
    obj match {
      case x: Tokens => x.tok1 == tok1 && x.tok2 == tok2
      case _ => super.equals(obj)
    }
  }

  def getIndex(tok: Pos) ={
    if (tok == tok1) 1
    else if (tok == tok2) 2
    else
      0
  }

  def get(index : Int) = {
    index match {
      case 1 => tok1
      case 2 => tok2
    }
  }

  def toList() = List(tok1.toList(), tok2.toList())

  def toSet() = Set((tok1,1),(tok2,2))

  def contains(tok: Pos) = tok == tok1 || tok == tok2
}



object Main extends App{

  def checking(before: Board, after: Board): Boolean = {
    val turIncreaseOne = before.turn - after.turn == -1
    val playerExchanged = before.player.card == after.enemy.card && before.enemy.card == after.player.card

    val myTokBefore = before.player.tokens
    val myTokAfter = after.enemy.tokens
    val enemyTokBefore = before.enemy.tokens
    val enemyTokAfter = after.player.tokens
    val card = before.player.card

    def winningMove(from:Pos, to : Pos) = {
      before.getPosHeight(to) == 3 && before.getPosHeight(from) != 3
    }

    def boardDiff() = {
      val diffPos = before.spaces.toSet diff after.spaces.toSet
      diffPos.map{
        case (pos,height) => {
          (pos,after.spaces.get(pos).get - before.spaces.get(pos).get)
        }
      }
    }

    def validateTok(from: Pos,to: Pos, card: String) = {
      // assert the position that
      // in move range && in the board && no tower built  && tower low enough && not occupied by other tokens
      val dist = from - to
      val inMoveRange = dist.row <= 1 && dist.row >= -1 && dist.col <= 1 && dist.col >= -1
      val towerLowEnough = before.getPosHeight(to) - before.getPosHeight(from) < 2
      val notOccupied = !before.isOccupied(tokPos = to)
      card match {
        case "Apollo" | "Minotaur" => inMoveRange && !to.outOfBound() && !before.isTowerBuilt(to) && towerLowEnough
        case _ => inMoveRange && !to.outOfBound() && !before.isTowerBuilt(to) && towerLowEnough && notOccupied

      }

    }

    def isValidTokenMove():Option[(Pos, Pos)] = {

      def defaultMove() = {
        val playerTokDiff = myTokBefore.toSet diff myTokAfter.toSet
        if(playerTokDiff.size == 1){
          val index = playerTokDiff.head._2
          if (validateTok(myTokBefore.get(index), myTokAfter.get(index),card))
            Some((myTokBefore.get(index),myTokAfter.get(index)))
          else None
        }
        else
          None

      }

      card match {
        case "Artemis" => {
          val playerTokDiff = myTokBefore.toSet diff myTokAfter.toSet
          if(playerTokDiff.size == 1){
            val index = playerTokDiff.head._2
            val from = myTokBefore.get(index)
            val to = myTokAfter.get(index)

            val tokFirstMoves = before.tokenMoves(from)
            if(tokFirstMoves.contains(to)){
              Some((myTokBefore.get(index),myTokAfter.get(index)))
            }else{
              val tokMoves : Set[(Pos,Pos)] = tokFirstMoves.filterNot(winningMove(from,_)).flatMap{
                firstMove => {
                  val secondMoves =  before.tokenMoves(firstMove).filterNot(_ == from)
                  secondMoves.map((firstMove,_)).filter{
                    case((firstMove,secondMove)) => { secondMove == to }
                  }
                }
              }
              val wonMoves = tokMoves.filter{
                case((fisrtMove,secondMove)) => winningMove(fisrtMove,secondMove)
              }
              if (wonMoves.nonEmpty)
                Some(wonMoves.head._1,wonMoves.head._2)
              else if (tokMoves.nonEmpty)
                Some(tokMoves.head._1,tokMoves.head._2)
              else None
            }


          }
          else
            None

        }
        case "Minotaur" => {
          if (enemyTokBefore == enemyTokAfter) { //enemy token unchanged
            defaultMove()
          }else{
            val playerTokDiff = myTokBefore.toSet diff myTokAfter.toSet
            if(playerTokDiff.size == 1){             // only one tok moved
              val enemyTokDiff = enemyTokBefore.toSet diff enemyTokAfter.toSet
              if (enemyTokDiff.size == 1){           // only one tok moved
                val myTokIdx = playerTokDiff.head._2
                val enemyTokIdx = enemyTokDiff.head._2
                if (myTokAfter.get(myTokIdx) == enemyTokBefore.get(enemyTokIdx)
                  && validateTok(playerTokDiff.head._1,enemyTokDiff.head._1,card)){ // valid move
                  val dir = enemyTokDiff.head._1 - playerTokDiff.head._1
                  if ((enemyTokBefore.get(enemyTokIdx) + dir == enemyTokAfter.get(enemyTokIdx))
                    && validateTok(enemyTokBefore.get(enemyTokIdx),enemyTokAfter.get(enemyTokIdx),"")){ // valid push result
                    Some((myTokBefore.get(myTokIdx),myTokAfter.get(myTokIdx)))
                  }else None
                }else
                  None
              }else
                None
            }else
              None

          }
        }
        case "Apollo" => {
          if (enemyTokBefore == enemyTokAfter) { //enemy token unchanged
            defaultMove()
          }
          else{
            val playerTokDiff = myTokBefore.toSet diff myTokAfter.toSet
            if(playerTokDiff.size == 1){             // only one tok moved
              val enemyTokDiff = enemyTokBefore.toSet diff enemyTokAfter.toSet
              if (enemyTokDiff.size == 1){           // only one tok moved
                val myTokIdx = playerTokDiff.head._2
                val enemyTokIdx = enemyTokDiff.head._2
                if (myTokAfter.get(myTokIdx) == enemyTokBefore.get(enemyTokIdx)
                  && validateTok(playerTokDiff.head._1,enemyTokDiff.head._1,card)
                  && myTokBefore.get(myTokIdx) == enemyTokAfter.get(enemyTokIdx)){ // valid push result
                    Some((myTokBefore.get(myTokIdx),myTokAfter.get(myTokIdx)))
                }else
                  None
              }else
                None
            }else
              None
          }
        }
        case _ => {
          if (enemyTokBefore == enemyTokAfter) { //enemy token unchanged
            defaultMove()
          }else
            None
        }
      }

    }

    def validateBuild(from:Pos, buildPos : (Pos,Int)) = {
      val to = buildPos._1
      val heightDiff = buildPos._2
      val dist = from - to
      val inBuildRange = dist.row <= 1 && dist.row >= -1 && dist.col <= 1 && dist.col >= -1
      val notOccupied = !after.isOccupied(tokPos =  to)
      before.player.card match{
        case _ =>  inBuildRange && heightDiff == 1 && notOccupied

      }
    }

    def validateDefault() = {
      isValidTokenMove() match {
        case Some((from,to)) => {
//          println("valid move")
          if (winningMove(from,to)){
            boardDiff().isEmpty
          }
          else{
            val diff = boardDiff()
            if(diff.size == 1){
              validateBuild(to,diff.head)
            }else
              false
          }
        }
        case None => false
      }
    }


    lazy val validPlay = before.player.card match {
      //swap token
      case "Apollo" => {
        validateDefault()
      }
      //double move
      case "Artemis" => {
        validateDefault()
      }
      // push a token back
      case "Minotaur" => {
        validateDefault()
      }
      // can build before moving, but then can only move to lower position
      case "Prometheus" => {
        def buildThenMove() ={

          val playerTokDiff = myTokBefore.toSet diff myTokAfter.toSet
          if(playerTokDiff.size == 1){
            val index = playerTokDiff.head._2
            val to = myTokAfter.get(index)
            val from = myTokBefore.get(index)
            if (validateTok(from, to,card) && before.getPosHeight(to) - before.getPosHeight(from) <= 0 ) {
              val diff = boardDiff()
              if (diff.size == 2){
                (validateBuild(from,diff.head) && validateBuild(myTokAfter.get(index),diff.last)
                || (validateBuild(from,diff.last) && validateBuild(myTokAfter.get(index),diff.head)))
              }
              else false
            }
            else false
          }
          else false

        }
        validateDefault() || buildThenMove()
      }
      // buid to 4
      case "Atlas" => {
        isValidTokenMove() match {
          case Some((from,to)) => {
            if (winningMove(from,to)){
              boardDiff().isEmpty
            }
            else{
              val diff = boardDiff()
              if(diff.size == 1){
                val from = to
                val buildTo = diff.head._1
                val heightDiff = diff.head._2
                val dist = from - buildTo
                val inBuildRange = dist.row <= 1 && dist.row >= -1 && dist.col <= 1 && dist.col >= -1
                inBuildRange && (heightDiff == 1 || after.spaces.get(buildTo).get == 4)
              }else
                false
            }
          }
          case None => false
        }
      }
      // build at 2 different positions
      case "Demeter" => {
        isValidTokenMove() match {
          case Some((from,to)) => {
            if (winningMove(from,to)){
              boardDiff().isEmpty
            }
            else{
              val diff = boardDiff()
              if(diff.size <= 2){
                validateBuild(to,diff.head) && validateBuild(to,diff.last)
              }else
                false
            }
          }
          case None => false
        }
      }
      // build same pos twice, cannot build to level 4
      case "Hephastus" => {
        isValidTokenMove() match {
          case Some((from,to)) => {
            if (winningMove(from,to)){
              boardDiff().isEmpty
            }
            else{
              val diff = boardDiff()
              if(diff.size == 1){
                val from = to
                val buildTo = diff.head._1
                val heightDiff = diff.head._2
                val dist = from - buildTo
                val inBuildRange = dist.row <= 1 && dist.row >= -1 && dist.col <= 1 && dist.col >= -1
                inBuildRange && ( heightDiff == 1 || (heightDiff == 2 && after.spaces.get(buildTo).get != 4))
              }else
                false
            }
          }
          case None => false
        }
      }
      case _ => {
        false
      }
    }

    //valid turn : assert turn + 1
    //valid player: assert player exchanged and the move of the token is valid
    //valid board: assert the build is valid
    turIncreaseOne && playerExchanged && validPlay
  }


  val parser = new Parser();

  while(true){
    //  read a JSON string, which is a brief description of the test case;
    val description = scala.io.StdIn.readLine()
    //  read a JSON board, which corresponds to the board state before a turn;
    val beforeBoard = scala.io.StdIn.readLine()
    //  read a second JSON board, which is a candidate board state after a turn; and
    val afterBoard = scala.io.StdIn.readLine()

    val before = parser.parse(beforeBoard)
    val after = parser.parse(afterBoard)

    //  print the JSON string back out for the test-case description;
    println(description)
    //  print the JSON string "ok" if the second board is a valid result from a player program that is given the first board, or print the JSON string "invalid" otherwise.
    if (checking(before,after))
      println("\"ok\"")
    else
      println("\"invalid\"")
  }




}
