package org.adamnew123456.scrabble

import scala.collection.mutable.HashMap
import scala.util.{Try, Success, Failure, Random}

/**
 * An exception for when somebody tries to take or swap more tiles than are
 * available.
 */
case class NoTilesError(tilesRequested: Int, tilesLeft: Int)
  extends Exception {
  override def toString = s"NoTilesError(requested: $tilesRequested, left: $tilesLeft)"
}

/**
 * The tile bag is responsible for holding the tiles in the game. It starts
 * off with a particular distribution of tiles, and can then have tiles taken
 * out, or swapped.
 */
class TileBag(tileDistribution: Map[Char, Int]) {
  val randomGen = new Random
  
  val tiles = new HashMap[Char, Int]()
  tiles ++= tileDistribution
  
  /**
   * Computes the number of remaining tiles.
   */
  def tilesLeft = tiles.values.sum
  
  /**
   * Whether or not any tiles are left.
   */
  def isEmpty = tilesLeft > 0
  
  /**
   * Draws the given number of tiles out of the bag. If there are too few, then
   * this produces a NoTilesError.
   */
  def drawTiles(numTiles: Int): Try[List[Char]] = {
    if (numTiles > tilesLeft) {
      Failure(NoTilesError(numTiles, tilesLeft))
    } else {
      val tilesToRemove = (1 to numTiles).map { _ =>
        val tilesCounts = tiles.toList
        
        /*
         *  Rather than having to deal with a list of (tile, count) pairs, go 
         *  ahead and expand the list so that way we can do clean indexing.
         */
        val expandedTiles = tiles.flatMap {case (tile, times) => List.fill(times)(tile)}
        val shuffledTiles = randomGen.shuffle(expandedTiles).toList
        
        val tileIndex = Math.abs(randomGen.nextInt % shuffledTiles.length)
        val letterPicked = shuffledTiles(tileIndex)
        tiles(letterPicked) -= 1
        letterPicked
      }
      
      Success(tilesToRemove.toList)
    }
  }
  
  /**
   * Puts the given tiles back into the bag, and pulls out new ones. Note that
   * the new tiles are pulled out before the old, to avoid getting the very
   * same tiles as before.
   */
  def replaceTiles(toReplace: List[Char]): Try[List[Char]] = {
    if (toReplace.length > tilesLeft) {
      Failure(NoTilesError(toReplace.length, tilesLeft))
    } else {
      val tilesToReturn = drawTiles(toReplace.length) match {
        case Success(tiles) => tiles
        
        // This cannot fail - the only way for this to occur is if there aren't
        // enough tiles, and we checked for that. This is really just to satisfy
        // the compiler.
        case Failure(_) => null
      }
      
      toReplace.foreach(tiles(_) += 1)
      Success(tilesToReturn)
    }
  }
}