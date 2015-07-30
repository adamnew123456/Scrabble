package org.adamnew123456.scrabble.players.swing

import java.util.concurrent.BlockingQueue
import javax.swing.{ BoxLayout, JButton, JPanel }

import org.adamnew123456.scrabble.{ Board, Config, EndGame, TileGroup, TurnBuilder, TurnRejectReason }

/**
 * This runs the UI - it handles requests from a SwingPlayer running in another
 * thread, and manipulates the various elements of the UI to provide input to
 * the Game.
 */
class UIManager(config: Config, returnQueue: BlockingQueue[UIMessage]) {
  val turnBuilder = new TurnBuilder(Board.empty(15, 15), TileGroup.fromTraversable(""))
  
  val boardSelection = new TileSelection()
  val replaceSelection = new MultiTileSelection()
  val messageReporter = new MessageReporter()
  
  val scoreView = new ScoreView()
  val rackView = new RackView(config, boardSelection, replaceSelection)
  val boardView = new BoardView(config, boardSelection, messageReporter, turnBuilder)
  val messageView = new SwingMessageReporter(messageReporter)

  turnBuilder.attachObserver(rackView.builderObserver)
  turnBuilder.attachObserver(boardView.builderObserver)

  // Get an initial display up and running
  rackView.builderObserver(turnBuilder)
  boardView.builderObserver(turnBuilder)
  
  // The current UI mode - this is required so that the submit button knows
  // what to do when it is clicked
  var mode: UIMode.Type = UIMode.Idle
  
  val submit = new JButton("Submit")
  submit.addActionListener(new ClosureActionListener({ _ =>
    println(s"Activated: $mode")
    mode match {
      case UIMode.Turn =>
        val additions = turnBuilder.getAdditions
        println(s"Turn: $additions")
        returnQueue.put(TurnMessage(additions))
        
      case UIMode.Replace =>
        val replacements = replaceSelection.get
        println(s"Replace: $replacements")
        returnQueue.put(TileReplaceMessage(replacements))

      case _ => ()
    }
    
    // Ensure that the user cannot activate the button, until the mode is
    // changed elsewhere
    println("OK")
    submit.setEnabled(false)
  }))

  // Set the initial modes, so that the player can't do anything before it is
  // their first turn
  submit.setEnabled(false)
  rackView.setMode(UIMode.Idle)
  boardView.setMode(UIMode.Idle)
  
  val window = new JPanel()
  window.setLayout(new BoxLayout(window, BoxLayout.Y_AXIS))
  
  window.add(rackView)
  window.add(boardView)
  window.add(scoreView)
  window.add(submit)
  window.add(messageView)
  
  def gameError(error: Throwable) {
    messageReporter.report(error.toString)
  }
 
  def replaceTilesMode(tiles: TileGroup, maxReplace: Int, failReason: Option[Throwable]) {
    boardView.setMode(UIMode.Replace)
    rackView.setMode(UIMode.Replace)
    mode = UIMode.Replace

    messageReporter.report("Replace your tiles")
    turnBuilder.reloadTiles(tiles)
    submit.setEnabled(true)
  }
  
  def turnMode(board: Board, tiles: TileGroup, failReason: Option[TurnRejectReason]) {
    boardView.setMode(UIMode.Turn)
    rackView.setMode(UIMode.Turn)
    mode = UIMode.Turn

    messageReporter.report("Do your turn")
    turnBuilder.reload(board, tiles)
    submit.setEnabled(true)
  }
  
  def otherPlayerTurn(player: String) {
    messageReporter.report(s"Current player: $player")
  }
  
  def startTurn(board: Board, tiles: TileGroup, scores: Map[String, Int]) {
    scoreView.update(scores)
    turnBuilder.reload(board, tiles)
  }
  
  def endTurn(board: Board, tiles: TileGroup, scores: Map[String, Int]) {
    boardView.setMode(UIMode.Idle)
    rackView.setMode(UIMode.Idle)
    mode = UIMode.Idle

    scoreView.update(scores)
  }
  
  def endGame(end: EndGame) {
    turnBuilder.reload(end.board, TileGroup.fromTraversable(""))
    scoreView.update(end.scores)
    
    messageReporter.report(s"End of game: ${end.reason.toString}")
    
    boardView.setMode(UIMode.Idle)
    rackView.setMode(UIMode.Idle)
  }
}
