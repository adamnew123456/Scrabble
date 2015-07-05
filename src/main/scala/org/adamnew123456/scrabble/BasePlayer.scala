package org.adamnew123456.scrabble

import scala.collection.mutable.HashMap
import scala.util.{Try, Success, Failure}

/**
 * An error raised when a player tries to use a tile that isn't in their hand.
 */
case class NoTileInHandError(tile: Char) extends Exception {
  override def toString = s"NoTileInHandError($tile)"
}

/**
 * This is mostly just a small interface for defining a player. It is only
 * capable of doing three things:
 * 
 * - Reacting to the start and end of a turn.
 * - Requesting or rejecting an offer to replace any tiles that the player
 *   wants replaced.
 *   
 * - Executing a turn with a given board and rack of tiles, returning a map
 *   of characters to add to the board.
 */
abstract class BasePlayer(val name: String, game: Config) {
  protected var scorer: WordScorer = _
  def setScorer(ws: WordScorer) = 
    scorer = ws

  /**
   * This determines whether or not the game should replace some of the
   * player's tiles with tiles drawn from the bag.
   * 
   * Note that any invalid requests will result in this function being
   * called again, on the same turn. If there aren't any tiles to replace,
   * then this function will not get called.
   */
  def replaceTiles(tiles: TileGroup, maxReplace: Int): TileGroup
  
  /**
   * This executes the turn, producing a map of the characters to add to the
   * board.
   * 
   * Note that providing an invalid set of moves will result in this function
   * being called again, on the same turn.
   */
  def turn(board: Board, tiles: TileGroup): Map[(Int, Int), Char]
  
  /**
   * This is executed at the beginning of a turn.
   */
  def startTurn(board: Board, tiles: TileGroup, score: Map[String, Int]): Unit
  
  /**
   * This is executed at the end of a turn.
   */
  def endTurn(board: Board, tiles: TileGroup, score: Map[String, Int]): Unit
  
  /**
   * This is executed at the end of the game.
   */
  def endGame(result: EndGame): Unit
}
