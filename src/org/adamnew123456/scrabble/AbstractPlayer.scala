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
 * This holds what is common to all players. Specifically:
 * 
 * - The player's current rack of tiles
 * - The player's current score
 */
abstract class AbstractPlayer(tileBag: TileBag) {
  var score = 0
  var tiles = tileBag.drawTiles(7) match {
    case Success(tiles) => tiles
    case Failure(exn)   => throw exn
  }
  
  /**
   * This executes the turn, producing a map of words to add at the given
   * positions.
   */
  def doTurn(board: Board): Map[(Int, Int, Direction.Type), String]
  
  /**
   * Adds the turn's score to the player's overall score.
   */
  def addScore(turnScore: Int) = score += turnScore
  
  /**
   * Counts the tiles that the player has, and returns a Map with the tiles
   * and their counts.
   */
  private def tilesToHashMap: HashMap[Char, Int] = {
    val tileCounts = new HashMap[Char, Int]()
    tiles.foreach { tile: Char =>
      if (!tileCounts.contains(tile)) {
        tileCounts(tile) = 0
      }
      tileCounts(tile) += 1
    }
    
    tileCounts
  }
  
  /**
   * Removes the tiles in the given list from a tile HashMap.
   */
  private def removeUsedTiles(toRemove: List[Char], tileMaps: HashMap[Char, Int]): Try[Unit] = {
    toRemove.foreach { tile: Char =>
      if (!tileMaps.contains(tile)) {
        return Failure(NoTileInHandError(tile))
      } else {
        tileMaps(tile) -= 1
        
        if (tileMaps(tile) < 0) {
          return Failure(NoTileInHandError(tile))
        }
      }
    }
    
    Success(())
  }
  
  /**
   * Converts a Map of character counts back to a list of characters.
   */
  def toCharacterList(map: Map[Char, Int]) =
    map.flatMap {case (tile, count) => List.fill(count)(tile)}
  
  /**
   * Consumes the given tiles, and replaces them with tiles from the
   * bag.
   */
  def useTiles(usedTiles: List[Char]): Try[Unit] = {
    val tileCounts = tilesToHashMap
    removeUsedTiles(usedTiles, tileCounts) match {
      case Success(_)   => ()
      case Failure(exn) => return Failure(exn)
    }
    
    val drawnTiles = tileBag.drawTiles(usedTiles.length) match {
      case Success(tiles) => tiles
      case Failure(exn)   => return Failure(exn)
    }
    
    val tilesNotRemoved = toCharacterList(tileCounts.toMap)
    tiles = tilesNotRemoved.toList ++ drawnTiles
    Success(())
  }
  
  /**
   * Swaps the input tiles with some tiles randomly chosen from the tile bag.
   */
  def swapTiles(swappedTiles: List[Char]): Try[Unit] = {
    val tileCounts = tilesToHashMap
    removeUsedTiles(swappedTiles, tileCounts) match {
      case Success(_)   => ()
      case Failure(exn) => return Failure(exn)
    }
    
    val drawnTiles = tileBag.replaceTiles(swappedTiles) match {
      case Success(tiles) => tiles
      case Failure(exn)   => return Failure(exn)
    }
    
    val tilesNotRemoved = toCharacterList(tileCounts.toMap)
    tiles = tilesNotRemoved.toList ++ drawnTiles
    Success(())
  }
}