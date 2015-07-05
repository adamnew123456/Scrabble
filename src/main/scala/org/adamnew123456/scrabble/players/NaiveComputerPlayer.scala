package org.adamnew123456.scrabble.players

import scala.annotation.tailrec
import scala.util.{Try, Success, Failure}

import org.adamnew123456.scrabble.{BasePlayer, Board, Config, EndGame, TileGroup, WordScorer}

/**
 * This is a fairly simple computer player, which will simply try to find the
 * best move it can within a certain timeframe, by plugging letters into
 * various tiles and seeing if they score.
 */
class NaiveComputerPlayer(name: String, game: Config, thinkTimeNano: Long = NaiveComputerPlayer.MAX_TIMEFRAME_NS)
    extends BasePlayer(name, game) {
  
  def replaceTiles(tiles: TileGroup, maxReplace: Int): TileGroup = {
    // This AI doesn't consider replacing tiles 
    TileGroup.fromTraversable(Nil)
  }
  
  def turn(board: Board, tiles: TileGroup): Map[(Int, Int), Char] = {
    // First, find out which tiles can have tiles added to them
    val squares = for {col <- 0.to(board.width - 1)
                       row <- 0.to(board.height - 1)}
                  yield {
                    // Check all the adjacent tiles, and see if any of them are
                    // occupied by a tile
                    val left = (col - 1, row)
                    val right = (col + 1, row)
                    val above = (col, row - 1)
                    val below = (col, row + 1)
                    
                    val adjacent = List(left, right, above, below).map(board.get)
                    if (adjacent.exists(_.isDefined)) {
                      Some((col, row))
                    } else {
                      None
                    }
                  }
                  
    val validSquares = squares.filter(_.isDefined).map(_.get)
    
    // These are the words on the board originally, for comparison
    val oldWords = board.findWords.map(_.text)
    
    val moveGenerator = new NaiveMoveGenerator(board, tiles, scorer, validSquares)
    val targetTime = System.nanoTime + thinkTimeNano
    moveGenerator.run(targetTime) match {
      case Some(moves) => moves
      case None        => Map[(Int, Int), Char]()
    }
  }
  
  def startTurn(board: Board, tiles: TileGroup, score: Map[String, Int]) = ()
  def endTurn(board: Board, tiles: TileGroup, score: Map[String, Int]) = ()
  def endGame(result: EndGame) = ()
}

object NaiveComputerPlayer {
  // How long the computer player has to find the best match it can - in
  // nanoseconds, since we use System.nanoTime because it is (usually)
  // monotonic
  val MAX_TIMEFRAME_NS: Long = (30 * 1e9).toLong
}
