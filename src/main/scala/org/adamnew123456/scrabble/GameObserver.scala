package org.adamnew123456.scrabble

/**
 * All the information about a player that is relevant to a GameObserver.
 */
case class PlayerInfo(tiles: TileGroup, score: Int)

/**
 * Observers are notified when various events in the game take place, and are
 * allowed to react to them, without having the ability to modify the state
 * of any of the players.
 */
trait GameObserver {
  /**
   * This is executed at the beginning of a player's turn.
   */
  def startTurn(board: Board, currentPlayer: String, players: Map[String, PlayerInfo]): Unit

  /**
   * This is executed at the end of a turn.
   */
  def endTurn(board: Board, currentPlayer: String, players: Map[String, PlayerInfo]): Unit

  /**
   * This is executed at the end of the game.
   */
  def endGame(result: EndGame): Unit
}