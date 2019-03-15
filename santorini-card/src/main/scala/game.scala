
case class Player(tokens: Tokens, card: String) {
  override def toString: String = "tokens: " + tokens.toString + "card: " + card.toString

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
    Pos(that.row - row, that.col - col)
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

  override def toString: String = {
    tok1.toString + "," + tok2.toString
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case x: Tokens => x.tok1 == tok1 && x.tok2 == tok2
      case _ => super.equals(obj)
    }
  }

  def getIndex(tok: Pos) = {
    if (tok == tok1) 1
    else if (tok == tok2) 2
    else
      0
  }

  def get(index: Int) = {
    index match {
      case 1 => tok1
      case 2 => tok2
    }
  }

  def toList() = List(tok1.toList(), tok2.toList())

  def toSet() = Set((tok1, 1), (tok2, 2))

  def contains(tok: Pos) = tok == tok1 || tok == tok2
}


object game extends App {

  val parser = new Parser()

  def printList(args: TraversableOnce[_]): Unit = {
    args.foreach(println)
  }

  //"spaces\":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]]}"
  //\"players\":[[[3,5],[2,5]],[[3,4],[4,4]]]

  def main(): Unit = {

    // [{"card":"Artemis"},{"card":"Prometheus"}]

    //val json_string = "{\"turn\":0,\"players\":[{\"tokens\":[[2,3],[4,4]],\"card\":\"Artemis\"},{\"tokens\":[[2,5],[3,5]],\"card\":\"Prometheus\"}],\"spaces\":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]]}"
    //json_string = "[{\"card\":\"Artemis\"},{\"card\":\"Prometheus\"}]"
    //json_string = "\n  [{\"card\":\"Prometheus\"},\n\n   {\"tokens\":[[2,3],[4,4]],\"card\":\"Artemis\"}]\n"

    while(true){
      val json_string = scala.io.StdIn.readLine()

      val game = parser.parse(json_string)
      val initPosition = List(Pos(3, 2), Pos(3, 3), Pos(3, 4), Pos(4, 4))

      if (game.spaces.isEmpty) {
        if (game.enemy.tokens != null) {
          val turn = 0
          val tokens = initPosition.filterNot(game.enemy.tokens.contains(_)).take(2)
          val player = Player(new Tokens(tokens(0),tokens(1)), game.player.card)
          val enemy = game.enemy
          val new_board = Board(turn, enemy, player, Map.empty[Pos, Int],true,None)
          parser.boardToJson(new_board)
        }
        else {
          val turn = 0
          val player = Player(new Tokens(Pos(3, 2), Pos(3, 3)), game.player.card)
          val enemy = game.enemy

          val new_board = Board(turn, enemy, player, Map.empty[Pos, Int],true,None)
          parser.boardToJson(new_board)
        }
      } else {
        game.player.card match {
//          //swap cards
//          case "Apollo" => {
//          }
//          //double move
//          case "Artemis" => {
//          }
//          // push a token back
//          case "Minotaur" => {
//          }
//          // can build before moving, but then can only move to lower position
//          case "Prometheus" => {
//          }
//          // buid to 4
//          case "Atlas" => {
//          }
//          // build at 2 different positions
//          case "Demeter" => {
//          }
//          // build same pos twice, cannot build to level 4
//          case "Hephastus" => {
//          }
          case _ => {
            //val new_board = game.moveThenBuild()
            val new_board = MCTS.findNextMove(game,1000)
            parser.boardToJson(new_board)
          }
        }
      }
    }


  }

  main()
}



