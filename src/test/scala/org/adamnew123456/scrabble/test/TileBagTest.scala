package org.adamnew123456.scrabble.test

import scala.util.{Try, Success, Failure}

import org.adamnew123456.scrabble.{TileBag, TileGroup, NoTilesError}

import org.junit.Test
import junit.framework.TestCase
import org.junit.Assert._

/**
 * Ensures that the tile bag works as intended.
 */
class TileBagTest extends TestCase {
  val tileDist = Map('a' -> 5, 'b' -> 10, 'c' -> 15)
  val numTiles = tileDist.values.sum
  var bag: TileBag = _
  
  override def setUp() {
    bag = new TileBag(tileDist)
  }
  
  /**
   * Ensures that the bag accurately reports the number of tiles inside of
   * it.
   */
  def testCountBag() {
    assertEquals(bag.tilesLeft, numTiles)
  }
  
  /**
   * Ensures that the random subsets drawn from the bag are different
   * subsets.
   */
  def testDraw() {
    val clonedBag = new TileBag(tileDist)
    
    val tilesA = bag.drawTiles(numTiles / 2) match {
      case Success(tiles) => tiles
      case Failure(exn) => 
        fail(s"Unexpected exception: $exn")
        null // Necessary for type checking - we want this to be inferred as
             // TileGroup, not as Any
    }
    
    val tilesB = clonedBag.drawTiles(numTiles / 2) match {
      case Success(tiles) => tiles
      case Failure(exn) => 
        fail(s"Unexpected exception: $exn")
        null
    }
    
    assertNotEquals(tilesA, tilesB)
  }
  
  /**
   * Ensure that drawing too many tiles results in an error.
   */
  def testDrawTooMany() {
    val tilesToDraw = numTiles * 2
    val overflow = tilesToDraw - numTiles
    
    bag.drawTiles(tilesToDraw) match {
      case Success(_) => fail("Expected NoTilesError")
      case Failure(NoTilesError(`tilesToDraw`, `numTiles`)) =>
        ()
      case Failure(exn) => 
        fail(s"Expecting NoTilesError($tilesToDraw, $numTiles), got $exn")
        
    }
  }
  
  /**
   * Ensure that tiles which are swapped can be drawn again.
   */
  def testSwap() {
    // Get rid of all tiles
    bag.drawTiles(numTiles - 1) match {
      case Success(_) => ()
      case Failure(exn) => fail(s"Unexpected exception: $exn")
    }
    
    assertEquals(bag.tilesLeft, 1)
    
    // Put one into the bag, and ensure we get that one out
    val toReplace = TileGroup.fromTraversable(List('a'))
    bag.replaceTiles(toReplace) match {
      case Success(tiles) => tiles
      case Failure(exn) =>
        fail(s"Unexpected exception: $exn")
        null
    }
    
    assertEquals(bag.tilesLeft, 1)
        
    val tilesDrawn = bag.drawTiles(1) match {
      case Success(tiles) => tiles
      case Failure(exn) =>
        fail(s"Unexpected exception: $exn")
        null
    }
    
    assertEquals(tilesDrawn.asList, List('a'))
  }
  
  /**
   * Ensure that swapping too many results in an error.
   */
  def testReplaceTooMany() {
    val numReplace = numTiles * 2
    val overflow = numReplace - numTiles
    val toReplace = TileGroup.fromTraversable(List.fill(numReplace)('x'))
    
    bag.replaceTiles(toReplace) match {
      case Success(_) => fail("Expected NoTilesError")
      case Failure(NoTilesError(`numReplace`, `numTiles`)) =>
        ()
      case Failure(exn) => 
        fail(s"Expecting NoTilesError($numReplace, $numTiles), got $exn")
        
    }
  }
}
