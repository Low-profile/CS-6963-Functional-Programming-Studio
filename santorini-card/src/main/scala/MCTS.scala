import scala.util.Random

/**
  * Represents a studied node.
  * @param board the [[Board]]
  * @param parent the parent node
  * @param subNodes the sub nodes
  * @param nbVisit the number of visit
  * @param score the score
  */
class Node(val board: Board, val parent: Node = null, var subNodes: Seq[Node] = Seq.empty, var nbVisit: Int = 0, var score: Int = 0)

/**
  * The Monte Carlo tree search implementation.
  */
object MCTS {
  /** The random instance. */
//  val random: Random = new Random(System.currentTimeMillis())
  val random: Random = new Random(10)
  /**
    * Find best next move from the specified board and the max duration.<br/>
    * Implements the Monte Carlo Tree Search
    * @param board the specified board
    * @param uctsTime the max duration
    * @return the best board
    */
  def findNextMove(board: Board, uctsTime: Int): Board = {
    val end = System.currentTimeMillis() + uctsTime
    val rootNode = new Node(board)
    var nbTurn = 0
    while (System.currentTimeMillis() < end) {
      nbTurn += 1
      val promiseNode = selectPromiseNode(rootNode)
      if (promiseNode.subNodes.isEmpty) {
        expandNode(promiseNode)
      }
      var nodeToExplore = promiseNode
      if (promiseNode.subNodes.nonEmpty) {
        nodeToExplore = promiseNode.subNodes(random.nextInt(promiseNode.subNodes.size))
      }
      val playoutResult = simulateRandomPlayout(nodeToExplore)
      backPropogation(nodeToExplore, playoutResult)
    }
    val bestNode = rootNode.subNodes.maxBy(n => n.score.toDouble / n.nbVisit)
    //logTree(rootNode)
    //Console.err.println("rootNode.subNodes :\n\t" + rootNode.subNodes.map(s => (s.score, s.nbVisit)).mkString("\n\t"))
    //Console.err.println("found " + bestNode.score + " / " + bestNode.nbVisit + " in " + nbTurn)
    bestNode.board
  }
  /**
    * Select the promise node = the node to study
    * @param rootNode the root node
    * @return the promise node
    */
  def selectPromiseNode(rootNode: Node): Node = {
    var node = rootNode
    while (node.subNodes.nonEmpty) {
      val parentVisitScore = math.log(node.nbVisit)
      node = node.subNodes.maxBy { subNode =>
        if (subNode.nbVisit == 0) {
          Int.MaxValue.toDouble
        } else {
          (1.41D * math.sqrt(parentVisitScore / subNode.nbVisit)) + (subNode.score.toDouble / subNode.nbVisit)
        }
      }
    }
    node
  }
  /**
    * Expand the specified node.
    * @param node the specified node
    */
  def expandNode(node: Node): Unit = {
    node.subNodes = node.board.getAllPossibleNextBoard
      .map{ board =>
        new Node(board, node)
      }
  }
  /**
    * Simulate a random game from the specified node.
    * @param node the specified node
    * @return the score of the random game
    */
  def simulateRandomPlayout(node: Node): Int = {
    if (node.board.optScoreIfFinal.exists(s => s < 0)) {
      node.parent.score = Int.MinValue
      -1
    } else {
      node.board.randomPlay(random)
    }
  }
  /**
    * Update tree node (nbVisit and score) from the specified node with the specified game result
    * @param node the specified node
    * @param playout the specified game result
    */
  def backPropogation(node: Node, playout: Int): Unit = {
    var currentNode = node
    while (currentNode != null) {
      currentNode.nbVisit += 1
      if (currentNode.score != Int.MinValue) {
        if (!currentNode.board.isEnemy) {
          if (playout < 0) {
            currentNode.score += 3
          } else if (playout > 0) {
            currentNode.score -= 1
          } else {
            currentNode.score += 1
          }
        } else {
          if (playout > 0) {
            currentNode.score += 3
          } else if (playout < 0) {
            currentNode.score -= 1
          } else {
            currentNode.score += 1
          }
        }
      }
      currentNode = currentNode.parent
    }
  }
  /**
    * Display in console.err the specified node with the specified prefix.
    * @param node the specified node
    * @param prefix the specified prefix
    */
  private def logTree(node: Node, prefix: String = ""): Unit = {
    Console.err.println(prefix + "(" + node.board.toString + ") " + node.score + "/" + node.nbVisit)
    val newPrefix = prefix + " "
    node.subNodes.foreach(sn => logTree(sn, newPrefix))
  }
}