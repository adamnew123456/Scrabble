package org.adamnew123456.scrabble.players.swing

import javax.swing.SwingUtilities
import org.adamnew123456.scrabble.{ Board, EndGame, GameObserver, PlayerInfo }

/**
 * This is used to communicate what player is currently active to the GUI.
 * This makes the UI more responsible, since it shows what is going on in
 * the game, instead of merely being disabled.
 */
class SwingObserver(uiManager: UIManager) extends GameObserver {
  def startTurn(board: Board, currentPlayer: String, players: Map[String, PlayerInfo]) {
    SwingUtilities.invokeLater(new ClosureRunnable({
      uiManager.otherPlayerTurn(currentPlayer)
    }))
  }

  def endTurn(board: Board, currentPlayer: String, players: Map[String, PlayerInfo]) = ()
  def endGame(result: EndGame) = ()
}