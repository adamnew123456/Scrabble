package org.adamnew123456.scrabble.test

import scala.util.{Try, Success, Failure}


import org.adamnew123456.scrabble.{
  AbstractPlayer, Board, Direction, NoTileInHandError, NoTilesError, TileBag}

import org.junit.Test
import junit.framework.TestCase
import org.junit.Assert._

class DummyPlayer(tileBag: TileBag) extends AbstractPlayer(tileBag) {
  def doTurn(board: Board): Map[(Int, Int, Direction.Type), String] = Map()
}

/**
 * Ensures that the AbstractPlayer works as intended.
 */
class AbstractPlayerTest extends TestCase {
  var player: DummyPlayer = _
  var tileBag: TileBag = _
  
  // An important property of this mapping is that it  has one of each tile,
  // which ensures that a tile which is swapped or removed never shows up again
  val tileDistribution = Map(
      'a' -> 1, 'b' -> 1, 'c' -> 1,
      'd' -> 1, 'e' -> 1, 'f' -> 1,
      'g' -> 1, 'h' -> 1, 'i' -> 1,
      'j' -> 1, 'k' -> 1, 'l' -> 1,
      'm' -> 1, 'n' -> 1, 'o' -> 1)
      
  val originalBagSize = tileDistribution.values.sum
  
  override def setUp() {
    tileBag = new TileBag(tileDistribution)
    player = new DummyPlayer(tileBag)
  }
  
  /**
   * Ensures that adding the score works correctly.
   */
  def testAddScore() {
    assertEquals(player.score, 0)
    player.addScore(5)
    assertEquals(player.score, 5)
  }
  
  /**
   * Ensures that removing players from the tiles hand actually removes them,
   * and that the player draws new tiles from the bag.
   */
  def testUseTiles() {
    val tilesToUse = player.tiles.take(3)
    
    player.useTiles(tilesToUse) match {
      case Success(())  => ()
      case Failure(exn) => fail(s"Unexpected exception: $exn")
    }
    
    // Make sure that the tiles we specified were removed...
    tilesToUse.foreach {tile: Char =>
      assertFalse(player.tiles.contains(tile))
    }
    
    // ... and that new tiles were added by taking them from the bag
    assertEquals(player.tiles.length, 7)
    assertEquals(tileBag.tilesLeft, originalBagSize - tilesToUse.length - 7)
  }
  
  /**
   * Ensures that swapping tiles gives different tiles back (by not 
   * "contaminating" the bag with the tiles to be swapped), and that
   * the bag has the same number of tiles it started with.
   */
  def testSwapTiles() {
    val tilesToSwap = player.tiles.take(3)
    player.swapTiles(tilesToSwap) match {
      case Success(())  => ()
      case Failure(exn) => fail(s"Unexpected exception: $exn")
    }
    
    // Make sure that the tiles we specified were removed...
    tilesToSwap.foreach {tile: Char =>
      assertFalse(player.tiles.contains(tile))
    }
    
    // ... and that new tiles were added, but that the tiles in the bag
    // were replaced
    assertEquals(player.tiles.length, 7)
    assertEquals(tileBag.tilesLeft, originalBagSize - 7)
    
    tilesToSwap.foreach {tile: Char =>
      assertEquals(tileBag.tiles(tile), 1)
    }
  }
}