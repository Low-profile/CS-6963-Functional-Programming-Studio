case class Board(turn: Int, player: Player, enemy: Player, spaces: Map[Pos, Int]) {
  def OutOfBound(tokPos: Pos): Boolean = {
    tokPos.outOfBound()
  }

  def isOccupied(player: Player = player, enemy: Player = enemy, tokPos: Pos) = {
    val mytokens = player.tokens
    val enemyTokens = enemy.tokens
    mytokens.contains(tokPos) || enemyTokens.contains(tokPos)
  }

  def canBePushedByMinotaur(player: Player, enemy: Player, tokPos: Pos) = {
    val mytokens = player.tokens
    mytokens.contains(tokPos)
  }

  def isTowerTooHigh(from: Pos, to: Pos) = {
    (spaces.get(to).get - spaces.get(from).get) > 1
  }

  def isTowerBuilt(tokPos: Pos) = {
    spaces.get(tokPos).get == 4
  }




  def assertNewTokenPos(
                         t: Pos,
                         predicates: List[(Pos) => Boolean]
                       ): Boolean = {
    if (predicates.isEmpty)
      false
    else {
      val head = predicates.head
      if (head(t)) {
        true
      }
      else
        assertNewTokenPos(t, predicates.tail)
    }

  }


  def tokenMoves(tokPos: Pos): Set[Pos] = {
    //val surroundings = directions.map(d => t.zip(d).map(x => x._1 + x._2))
    val surroundings = tokPos.getSurroundings()

    val mycard = player.card

    mycard match {

      //todo
      //push somebody back
      case "Minotaur" => {
        surroundings.filterNot { to => {
          assertNewTokenPos(
            to,
            List(OutOfBound, isTowerBuilt)
          ) || isTowerTooHigh(tokPos, to) || isOccupied(tokPos = to)
        }
        }.toSet
      }
      // switch token
      //      case "Apollo" => {
      //        surroundings.filterNot { l => {
      //          val r = l.row - 1
      //          val c = l.col - 1
      //          assertNewTokenPos(
      //            tokPos,
      //            r,
      //            c,
      //            List(OutOfBound, isTowerBuilt, isTowerTooHigh)
      //          )
      //        }
      //        }
      //      }
      case _ => {
        surroundings.filterNot { to => {
          assertNewTokenPos(
            to,
            List(OutOfBound, isTowerBuilt)
          ) || isTowerTooHigh(tokPos, to) || isOccupied(tokPos = to)
        }
        }.toSet
      }
    }

  }

  def moveBuild(tokIdx: Int, movePos: Pos): List[Pos] = {
    val surroundings = movePos.getSurroundings()
    val newPlayer: Player = player.moveTokenTo(tokIdx, movePos)

    surroundings.filterNot { l => {
      assertNewTokenPos(
        l,
        List(OutOfBound, isTowerBuilt)
      ) || isOccupied(tokPos = l)
    }
    }
  }


  def getPosHeight(pos: Pos) = {
    spaces.get(pos) match {
      case Some(i) => i
      case None => 0
    }
  }

}
