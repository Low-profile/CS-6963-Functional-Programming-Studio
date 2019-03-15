import spray.json._

class Parser {
  type Space = Map[Pos, Int]

  object MyJsonProtocol extends DefaultJsonProtocol {

    implicit object TokensJsonFormat extends RootJsonFormat[Tokens] {
      def write(c: Tokens) =
        JsArray(Vector(
          JsArray(JsNumber(c.tok1.row), JsNumber(c.tok1.col)),
          JsArray(JsNumber(c.tok2.row), JsNumber(c.tok2.col))))

      def read(value: JsValue) = {
        value match {
          case JsArray(Vector(JsArray(tok1), JsArray(tok2))) => {
            val tokPos1 = tok1 match {
              case Vector(JsNumber(row), JsNumber(col)) =>
                new Pos(row.toInt, col.toInt)
              case _ => throw new DeserializationException("Token expected")
            }
            val tokPos2 = tok2 match {
              case Vector(JsNumber(row), JsNumber(col)) =>
                new Pos(row.toInt, col.toInt)
              case _ => throw new DeserializationException("Token expected")
            }
            new Tokens(tokPos1, tokPos2)
          }
          case _ => {
            throw new DeserializationException("Token expected")
          }
        }
      }
    }

    implicit object PlayerJsonFormat extends RootJsonFormat[Player] {
      def write(p: Player) =
        if (p.tokens == null){
          JsObject(
            "card" -> JsString(p.card)
          )
        }
        else{
          JsObject(
            "tokens" -> p.tokens.toJson,
            "card" -> JsString(p.card)
          )
        }


      def read(value: JsValue) = {
        value.asJsObject.getFields("tokens", "card") match {
          case Seq(tokens, JsString(card)) =>{
            new Player(tokens.convertTo[Tokens], card)
          }
          case _ => throw new DeserializationException("Player expected")
        }
      }
    }

    implicit object SpaceJsonFormat extends RootJsonFormat[Space] {

      def spaceToList(s: Space) = {
        def updateSpace(b: List[List[Int]], s: Space): List[List[Int]] = {
          if (s.isEmpty)
            b
          else {
            val pop = s.head
            val r = pop._1.row - 1
            val c = pop._1.col - 1
            val h = pop._2
            val newB = b.updated(
              r,
              b(r)
                .updated(c, h)
            )
            updateSpace(newB, s.tail)
          }
        }

        val ret = List.fill(5)(List.fill(5)(0))
        updateSpace(ret, s)
      }

      def f(zipped: (List[Int], Int)) = {
        val row = zipped._1
        val rIdx = zipped._2 + 1

        def g(zipped: (Int, Int)) = {
          val height = zipped._1
          val cIdx = zipped._2 + 1
          val pos = new Pos(rIdx, cIdx)
          (pos, height)
        }

        row.zipWithIndex.map(x => g(x))
      }

      def write(s: Space) =
        spaceToList(s).toJson

      def read(value: JsValue) = {
        value match {
          case JsArray(Vector(a, b, c, d, e)) =>
            value.convertTo[List[List[Int]]].zipWithIndex.flatMap(x => f(x)).toMap
          case _ => throw new DeserializationException("Player expected")
        }
      }
    }

    implicit object BoardJsonFormat extends RootJsonFormat[Board] {

      def write(b: Board) = {
        if (b.spaces.isEmpty)
          JsArray(Vector(b.player.toJson,b.enemy.toJson))
        else
          JsObject(
            "turn" -> JsNumber(b.turn),
            "players" -> JsArray(b.player.toJson, b.enemy.toJson),
            "spaces" -> b.spaces.toJson
          )
      }


      def read(value: JsValue) = {
        value match {
          case JsArray(Vector(player, enemy)) => {
            val playerCard = player.asJsObject.getFields("card") match {
              case Seq(JsString(card)) => card
              case _ => throw new DeserializationException("Player expected")
            }
            val enemyCard = enemy.asJsObject.getFields("card","tokens") match {
              case Seq(JsString(card),tokens) => Player(tokens.convertTo[Tokens],card)
              case Seq(JsString(card)) => Player(null,card)
              case _ => throw new DeserializationException("Player expected")
            }
            Board(0, Player(null, playerCard), enemyCard, Map.empty[Pos, Int], false, None)
          }
          case other => {
            other.asJsObject.getFields("turn", "players", "spaces") match {
              case Seq(JsNumber(turn), JsArray(Vector(player, enemy)), spaces) =>
                new Board(turn.toInt, player.convertTo[Player], enemy.convertTo[Player], spaces.convertTo[Space], false,None)
              case _ => throw new DeserializationException("Player expected")
            }
          }

        }
      }

    }

  }
  import MyJsonProtocol._

  def parse(input: String) = {
    val jsonAst = input.parseJson
    jsonAst.convertTo[Board]
  }
  def boardToJson(b: Board): Unit ={
    println(b.toJson)
  }

}
