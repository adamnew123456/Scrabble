package org.adamnew123456.scrabble.players

import scala.annotation.tailrec
import scala.util.{ Try, Success, Failure }

import org.adamnew123456.scrabble._

/**
 * This is a fairly simple computer player, which will simply try to find the
 * best move it can within a certain timeframe, by plugging letters into
 * various tiles and seeing if they score.
 */
class NaiveComputerPlayer(name: String, game: Config, thinkTimeNano: Long = NaiveComputerPlayer.MAX_TIMEFRAME_NS)
    extends BasePlayer(name, game) {

  def replaceTiles(tiles: TileGroup, maxReplace: Int, failReason: Option[Throwable]): TileGroup = {
    // This AI doesn't consider replacing tiles 
    TileGroup.fromTraversable(Nil)
  }

  def turn(board: Board, tiles: TileGroup, failReason: Option[TurnRejectReason]): Map[(Int, Int), Char] = {
    println(s"[Naive $name] Starting turn")
    if (failReason.isDefined) {
      println(s"[Naive $name] Failure: ${failReason.get}")
    }

    // These are the words on the board originally, for comparison
    val oldWords = board.findWords.map(_.text)
    println(s"[Naive $name] Starting out with words $oldWords")

    val moveGenerator = new NaiveMoveGenerator(board, tiles, scorer)

    println(s"[Naive $name] Thinking for ${thinkTimeNano / 1e9} seconds.")
    val targetTime = System.nanoTime + thinkTimeNano
    moveGenerator.run(targetTime) match {
      case Some(moves) =>
        println(s"[Naive $name] Found valid move $moves")
        moves
      case None =>
        println(s"[Naive $name] Out of ideas")
        Map[(Int, Int), Char]()
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
