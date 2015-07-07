package org.adamnew123456.scrabble.players

import com.twitter.util.LruMap
import scala.collection.mutable.HashMap
import scala.util.{ Try, Success, Failure }
import org.adamnew123456.scrabble.{ Board, TileGroup, WordScorer }

/**
 * This generates the moves for the NaiveComputerPlayer - the procedure here is
 * complex enough that it was worthwhile to extract it from the
 * NaiveComputerPlayer.
 */
class NaiveMoveGenerator(board: Board, tiles: TileGroup, scorer: WordScorer) {
  /**
   * To define what moves should be possible for a turn, the following
   * structure is going to be used:
   *
   * - A Move(column, row, tile) is used to indicate that a tile is placed at
   *   the given location
   * - An And(moveA, moveB) is used to combine a pair of moves, doing them both
   *   in the same turn.
   *
   * This makes it easy to combine multiple moves per turn, if that would be
   * advantageous in terms of points.
   */
  trait Strategy
  case class Move(column: Int, row: Int, tile: Char) extends Strategy
  case class And(first: Strategy, second: Strategy) extends Strategy

  // This caches the available tiles, based upon what other tiles have been
  // used
  val tileCache = new LruMap[Set[(Int, Int)], IndexedSeq[(Int, Int)]](500)

  // Computes the open spaces for a given strategy
  def computeOpenSpaces(usedSpaces: Set[(Int, Int)]): IndexedSeq[(Int, Int)] = {
    if (tileCache.contains(usedSpaces)) {
      tileCache(usedSpaces)
    } else {
      val openSpacesOptions =
        for {
          col <- 0.to(board.width - 1)
          row <- 0.to(board.height - 1)
        } yield {
          val left = (col - 1, row)
          val right = (col + 1, row)
          val above = (col, row - 1)
          val below = (col, row + 1)

          val space = (col, row)
          val adjacent = List(left, right, above, below)
          if (adjacent.exists(usedSpaces.contains(_)) && !usedSpaces.contains(space)) {
            Some((col, row))
          } else {
            None
          }
        }

      val openSpaces = openSpacesOptions.filter(_.isDefined).map(_.get)
      tileCache(usedSpaces) = openSpaces
      openSpaces
    }
  }

  // Finds out what tiles would be used by a given strategy
  def findTilesUsed(strategy: Strategy): TileGroup =
    strategy match {
      case Move(_, _, tile) => TileGroup.fromTraversable(List(tile))
      case And(a, b)        => findTilesUsed(a).merge(findTilesUsed(b))
    }

  // Finds the spaces used by a given strategy
  def findSpacesUsed(strategy: Strategy): Set[(Int, Int)] =
    strategy match {
      case Move(col, row, _) => Set((col, row))
      case And(a, b)         => findSpacesUsed(a) ++ findSpacesUsed(b)
    }

  /*
   * With strategies defined, an implementation strategy jumps out:
   * 
   * - Have one part generate a batch of move strategies, as necessary.
   * - Have another part translate those strategies into moves.
   * - Have a final part which takes those moves and either discards them, or
   *   promotes them. Discarding can happen when the strategy isn't the best,
   *   or it is invalid. Promotion happens when the strategy is the best 
   *   strategy of all strategies seen so far.
   */

  /**
   * This combinatorially generates moves. It will either generate a fresh list
   * of Move objects if not given a seed, or will add onto the given seeds
   * by combining them with the And combinator.
   */
  def moveGenerator(seeds: List[Strategy] = Nil): Stream[Strategy] = {
    if (seeds == Nil) {
      // Simply take all the spaces, and all the tiles, and jam each possible
      // tile into each possible space
      val availableTiles = tiles.asList.toSet

      val moves = for {
        (col, row) <- computeOpenSpaces(board.usedSpaces)
        tile <- availableTiles
      } yield Move(col, row, tile)

      // This ensures that the move generator starts at the center if the 
      // Board is empty. We can ignore this issue in the later move generator,
      // since it would have already filled the center and branched out to
      // other tiles.
      val totalMoves =
        if (board.get(board.center).isDefined) {
          moves
        } else {
          val (centerCol, centerRow) = board.center
          availableTiles.map(Move(centerCol, centerRow, _))
        }

      totalMoves.toStream #::: moveGenerator(totalMoves.toList)
    } else {
      // If we've got a previous seed, then start working combinatorially and
      // generate moves
      val moves = seeds.flatMap { oldMove: Strategy =>
        val tilesUsed = findTilesUsed(oldMove)
        val tilesLeft = tiles.remove(tilesUsed)

        // I *could* have squeezed this into a for comprehension, but it would
        // have been messy expressing everything in terms of List
        tilesLeft match {
          case Success(freeTiles) =>
            val uniqueFreeTiles = freeTiles.asList.toSet
            uniqueFreeTiles.toList.flatMap { freeTile: Char =>
              val spacesUsed = findSpacesUsed(oldMove)
              val freeSpaces = computeOpenSpaces(board.usedSpaces ++ spacesUsed)

              freeSpaces.toList.map {
                case (col: Int, row: Int) => And(oldMove, Move(col, row, freeTile))
              }
            }

          case Failure(_) =>
            List()
        }
      }

      moves.toStream #::: moveGenerator(moves)
    }
  }

  // Converts a strategy into an actual move Map
  def toMove(strategy: Strategy): Map[(Int, Int), Char] =
    strategy match {
      case Move(col, row, tile) => Map((col, row) -> tile)
      case And(a, b)            => toMove(a) ++ toMove(b)
    }

  // This processes a move, returning either a Some of the move and the score, 
  // or None if the move is invalid
  def processMove(strategy: Strategy): Option[(Int, Map[(Int, Int), Char])] = {
    val move = toMove(strategy)
    val oldWords = board.findWords

    val turnScore = for {
      newBoard <- board.addCharacters(move)
      newWords <- Success(newBoard.findWords)

      wordDiff <- Success(scorer.computeModifiedWords(oldWords, newWords))
      score <- scorer.computeTurnScore(wordDiff)
    } yield score

    turnScore match {
      case Success(score) => Some(score, move)
      case Failure(_)     => None
    }
  }

  // This runs down the stream of generated moves, and finds the best one while
  // respecting the time limit we're given
  def run(maxTime: Long): Option[Map[(Int, Int), Char]] = {
    var bestMove: Option[Map[(Int, Int), Char]] = None
    var bestMoveScore = 0
    val strategies = moveGenerator()
    strategies.foreach { strategy: Strategy =>
      val time = System.nanoTime
      if (time >= maxTime) {
        println(s"[NaiveMoveGen] Best move is $bestMove")
        return bestMove
      } else {
        processMove(strategy) match {
          // Only record a score that is the best score seen so far
          case Some((score, move)) =>
            if (score > bestMoveScore) {
              bestMove = Some(move)
              bestMoveScore = score
            }
          case None => ()
        }
      }
    }

    bestMove
  }
}