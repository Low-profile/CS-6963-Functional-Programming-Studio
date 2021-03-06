case class Board(turn: Int, player: Player, enemy: Player, spaces: Map[Pos, Int]) {
  def OutOfBound(tokPos: Pos): Boolean = {
    tokPos.outOfBound()
  }

  def isOccupied(player: Player = player, tokPos: Pos) = {
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
            List(OutOfBound, isTowerBuilt )
          ) || isTowerTooHigh(tokPos,to)|| isOccupied(player, to)
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
            List(OutOfBound, isTowerBuilt )
          ) || isTowerTooHigh(tokPos,to)|| isOccupied(player, to)
        }
        }.toSet
      }
    }

  }

  def availableMove(): (Set[Pos], Set[Pos], Boolean) = {

    def containWinningMove(tokIdx: Int, moves: Set[Pos]): Option[(Set[Pos], Set[Pos], Boolean)] = {
      val winMoves = moves.filter(spaces.get(_).get == 3)
      if (!winMoves.isEmpty) {
        if (tokIdx == 1)
          Some((winMoves, Set.empty[Pos], true))
        else
          Some((Set.empty[Pos], winMoves, true))
      }
      else
        None
    }

    val mytokens = player.tokens
    val mycard = player.card

    mycard match {
      case "Artemis" => {
        //double move
        val tok1 = mytokens.tok1
        val tok1FirstMoves = tokenMoves(tok1)

        containWinningMove(1, tok1FirstMoves) match {
          case Some(i) => i
          case None => {
            val tok1Moves = tok1FirstMoves.flatMap(tokenMoves(_)).filterNot(_ == tok1)

            containWinningMove(1, tok1Moves) match {
              case Some(i) => i
              case None => {
                val tok2 = mytokens.tok2
                val tok2FirstMoves = tokenMoves(tok2)

                containWinningMove(2, tok2FirstMoves) match {
                  case Some(i) => i
                  case None => {
                    val tok2Moves = tok2FirstMoves.flatMap(tokenMoves(_)).filterNot(_ == tok2)

                    containWinningMove(2, tok2Moves) match {
                      case Some(i) => i
                      case None => {
                        (tok1Moves, tok2Moves, false)
                      }
                    }
                  }
                }
              }
            }
          }


        }
      }
      case _ => {
        val tok1Moves = tokenMoves(mytokens.tok1)
        containWinningMove(1, tok1Moves) match {
          case Some(i) => i
          case None => {
            val tok2Moves = tokenMoves(mytokens.tok2)
            containWinningMove(2, tok2Moves) match {
              case Some(i) => i
              case None => (tok1Moves, tok2Moves, false)
            }
          }
        }
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
      ) || isOccupied(newPlayer, l)
    }
    }
  }

  def movesZipWithBuids(tokIdx: Int, moves: Set[Pos]) = {
    moves.flatMap(movePos => {
      val builds = moveBuild(tokIdx, movePos)
      builds.map((movePos, _))
    })
  }

  def availableBuild(
                      moves: (Set[Pos], Set[Pos])
                    ): (Set[(Pos, Pos)], Set[(Pos, Pos)]) = {
    val tok1Builds = movesZipWithBuids(1, moves._1)
    val tok2Builds = movesZipWithBuids(2, moves._2)

    (tok1Builds, tok2Builds)
  }

  def exchangePlayer(p: List[Player]): List[Player] = {
    List(p(1), p(0))
  }

  def makeStep(nextStep: (Pos, Pos), tokenIdx: Int): Board = {
    val new_turn = turn + 1

    val move = nextStep._1

    val card = player.card

    val newEnemy = player.moveTokenTo(tokenIdx, move)
    val newPlayer = enemy

    val build = nextStep._2
    val originalTowerHeight = spaces.get(build).get
    val space = spaces.updated(build, originalTowerHeight + 1)

    Board(new_turn, newPlayer, newEnemy, space)
  }

  def makeWinningMove(to: Pos, tokenIdx: Int): Board = {
    val new_turn = turn + 1
    val card = player.card

    val newEnemy = player.moveTokenTo(tokenIdx, to)
    val newPlayer = enemy
    Board(new_turn, newPlayer, newEnemy, spaces)
  }

  def moveThenBuild() = {
    val potentialMoves = availableMove()
    if (potentialMoves._3) {
      val new_board =
        if (potentialMoves._1.isEmpty)
          makeWinningMove(potentialMoves._2.head, 2)
        else
          makeWinningMove(potentialMoves._1.head, 1)
      new_board
    }
    else {
      val steps = availableBuild((potentialMoves._1, potentialMoves._2))
      val new_board =
        if (steps._1.isEmpty)
          makeStep(steps._2.head, 2)
        else
          makeStep(steps._1.head, 1)
      new_board
    }
  }

  def getPosHeight(pos:Pos) ={
    spaces.get(pos) match {
      case Some(i) => i
      case None => 0
    }
  }

}
