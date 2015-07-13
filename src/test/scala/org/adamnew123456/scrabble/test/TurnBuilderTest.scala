package org.adamnew123456.scrabble.test
    

import scala.util.{ Try, Success, Failure }
import org.adamnew123456.scrabble._

import org.junit.Test
import junit.framework.TestCase
import org.junit.Assert._

/**
 * Tests the TurnBuilder, which is responsible for interactively composing
 * turns, while allowing 
 */
class TurnBuilderTest extends TestCase {
  /**
   * Ensures that the TurnBuilder pushes out a notification to its observers.
   */
  class NotifyTester {
    var wasNotified: Boolean = false
    
    val observe = { (builder: TurnBuilder) =>
      wasNotified = true
    }
  }
  
  def testSuccessfulAddTiles = {
    /*
     * 0 1 2 3 4
     * _ _ _ _ _ 0
     * _ _ r _ _ 1 
     * _ _ a _ _ 2
     * _ _ y _ _ 3
     * _ _ _ _ _ 4
     */
    val board = Board.empty(5, 5).addWord("ray", (2, 1), Direction.Vertical) match {
      case Success(board) => board
      case Failure(exn) => 
        fail(s"Unexpected error: $exn")
        null
    }
    
    val rack = TileGroup.fromTraversable("abcd")
    
    val observer = new NotifyTester
    val builder = new TurnBuilder(board, rack)
    builder.attachObserver(observer.observe)
    
    val tiles = Map((1, 2) -> 'b', (3, 2) -> 'd')
    builder.addTiles(tiles) match {
      case Success(())   => ()
      case Failure(exn)  => fail(s"Unexpected exception: $exn")
    }
    
    assertTrue(observer.wasNotified)
    
    // Ensure that the tiles were, in fact, added, and the tiles were removed
    val builderBoard = builder.getBoard
    val builderRack = builder.getTiles
    
    assertEquals(builderBoard.findWords,
        Set(Word("ray", Direction.Vertical, (2, 1)),
            Word("bad", Direction.Horizontal, (1, 2))))
    
    // Note that, because the board already had an 'a', we didn't use the one
    // in our rack
    assertEquals(builderRack.asMap, Map('a' -> 1, 'c' -> 1))
  }
  
  /**
   * This tests that trying to add tiles which already exist on the board, and
   * inside the turn builder, result in the correct errors.
   */
  def testDuplicateAddTiles {
    /*
     * 0 1 2 3 4
     * _ _ _ _ _ 0
     * _ _ r _ _ 1 
     * _ _ a _ _ 2
     * _ _ y _ _ 3
     * _ _ _ _ _ 4
     */
    val board = Board.empty(5, 5).addWord("ray", (2, 1), Direction.Vertical) match {
      case Success(board) => board
      case Failure(exn) => 
        fail(s"Unexpected error: $exn")
        null
    }
    
    val rack = TileGroup.fromTraversable("abcd")
    val observer = new NotifyTester
    val builder = new TurnBuilder(board, rack)
    builder.attachObserver(observer.observe)
    
    // First, test adding duplicate tiles that are on the board
    builder.addTiles(Map((2, 2) -> 'a')) match {
      case Success(_) => 
        fail("Expected DuplicateTilesError")
      case Failure(exn: DuplicateTilesError) =>
        assertEquals(exn.tiles, Set((2, 2)))
      case Failure(exn) => 
        fail(s"Expected DuplicateTilesError, got $exn")
    }
    
    // Ensure that the observer wasn't triggered, and that the board and rack
    // are unchanged
    assertFalse(observer.wasNotified)
    assertEquals(builder.getAdditions, Map())
    assertEquals(builder.getTiles, rack)
    
    // Next, test adding duplicate tiles inside the builder. Before that, we 
    // need to add a valid tile that we can try to overwrite
    builder.addTiles(Map((1, 2) -> 'a')) match {
      case Success(_)   => ()
      case Failure(exn) => fail(s"Unexpecte exception: $exn")
    }
    
    // Reset the observer, since it was just notified
    
    observer.wasNotified = false
    
    builder.addTiles(Map((1,2) -> 'b')) match {
      case Success(_)   => fail(s"Exepcted DuplicateTilesError")
      case Failure(exn: DuplicateTilesError) =>
        assertEquals(exn.tiles, Set((1, 2)))
      case Failure(exn) =>
        fail(s"Expected DuplicateTilesError, got $exn")
    }
    
    // Ensure that the observer wasn't triggered, and that the board has only
    // the original (1, 2) addition
    assertFalse(observer.wasNotified)
    assertEquals(builder.getAdditions, Map((1, 2) -> 'a'))
    assertEquals(builder.getTiles, TileGroup.fromTraversable("bcd"))
  }
  
  /**
   * This tests trying to add tiles that don't exist in the rack.
   */
  def testNotOnRackAddTiles {
    val board = Board.empty(5, 5)    
    val rack = TileGroup.fromTraversable("abcd")
    
    val observer = new NotifyTester
    val builder = new TurnBuilder(board, rack)
    builder.attachObserver(observer.observe)
    
    builder.addTiles(Map((1, 2) -> 'z')) match {
      case Success(_)   => fail(s"Expected NotInTileGroup")
      case Failure(exn: NotInTileGroup) =>
        assertEquals(exn.tiles, TileGroup.fromTraversable(List('z')))
      case Failure(exn) => fail(s"Expected NotInTileGroup, got $exn")
    }
    
    assertFalse(observer.wasNotified)
    assertEquals(builder.getAdditions, Map())
    assertEquals(builder.getTiles, rack)
  }
  
  /**
   * This tests that trying to add a tile to the builder which isn't on the
   * board, fails.
   */
  def testNotOnBoardAddTiles {
    val board = Board.empty(5, 5)    
    val rack = TileGroup.fromTraversable("abcd")
    
    val observer = new NotifyTester
    val builder = new TurnBuilder(board, rack)
    builder.attachObserver(observer.observe)
    
    builder.addTiles(Map((-1, -1) -> 'a')) match {
      case Success(_)   => fail(s"Expected NotOnBoardError")
      case Failure(exn: NotOnBoardError) =>
        assertEquals(exn.col, -1)
        assertEquals(exn.row, -1)
      case Failure(exn) => fail(s"Expected NotOnBoardError, got $exn")
    }
  }
  
  /**
   * This tests that trying to remove tiles from the builder, successfully.
   * 
   */
  def testRemoveTiles {
    val board = Board.empty(5, 5)
    val rack = TileGroup.fromTraversable("abcd")
    
    val observer = new NotifyTester
    val builder = new TurnBuilder(board, rack)
    builder.attachObserver(observer.observe)
    
    // Get a tile on the board that we can remove
    
    builder.addTiles(Map((0, 0) -> 'a', (0, 1) -> 'b')) match {
      case Success(_)   => ()
      case Failure(exn) => fail(s"Unexpected error: $exn")
    }
    
    // Needs to be reset so that we can ensure that removeTiles notifies
    observer.wasNotified = false
    
    // Now, try to remove one of them
    builder.removeTiles(Set((0, 0))) match {
      case Success(_)   => ()
      case Failure(exn) => fail(s"Unexpected error: $exn")
    }
    
    // Ensure that the observer was notified, that the tile was removed, and
    // that the 'a' is back in the rack
    assertTrue(observer.wasNotified)
    assertEquals(builder.getAdditions, Map((0, 1) -> 'b'))
    assertEquals(builder.getTiles, TileGroup.fromTraversable("acd"))
  }
  
  /**
   * This tests trying to remove tiles that are not in the builder - either
   * that exist on the board (they're permanent), or they're blank on both
   * the board and the builder.
   */
  def testBadRemoveTiles {
    /*
     * 0 1 2 3 4
     * _ _ _ _ _ 0
     * _ _ r _ _ 1 
     * _ _ a _ _ 2
     * _ _ y _ _ 3
     * _ _ _ _ _ 4
     */
    val board = Board.empty(5, 5).addWord("ray", (2, 1), Direction.Vertical) match {
      case Success(board) => board
      case Failure(exn) => 
        fail(s"Unexpected error: $exn")
        null
    }
    
    val rack = TileGroup.fromTraversable("abcd")
    
    val observer = new NotifyTester
    val builder = new TurnBuilder(board, rack)
    builder.attachObserver(observer.observe)
    
    // Ensure that removing tiles on the underlying board fails
    builder.removeTiles(Set((2, 2))) match {
      case Success(_)   => fail("Expected PermanentTileError")
      case Failure(exn: PermanentTileError) =>
        assertEquals(exn.col, 2)
        assertEquals(exn.row, 2)
      case Failure(exn) => fail(s"Expected PermanentTileError, got $exn")
    }
    
    // Check that the observer wasn't called, and nothing was changed
    assertFalse(observer.wasNotified)
    assertEquals(builder.getAdditions, Map())
    assertEquals(builder.getTiles, rack)
    
    // Now, test for tiles that are not on the board or the builder
    builder.removeTiles(Set((0, 0))) match {
      case Success(_)   => fail("Expected PermanentTileError")
      case Failure(exn: PermanentTileError) =>
        assertEquals(exn.col, 0)
        assertEquals(exn.row, 0)
      case Failure(exn) => fail(s"Expected PermanentTileError, got $exn")
    }
    
    // Check that the observer wasn't called, and nothing was changed
    assertFalse(observer.wasNotified)
    assertEquals(builder.getAdditions, Map())
    assertEquals(builder.getTiles, rack)
  }
  
  /**
   * The reload function should undo any changes, and trigger the observers.
   */
  def testReload {
    /*
     * 0 1 2 3 4
     * _ _ _ _ _ 0
     * _ _ r _ _ 1 
     * _ _ a _ _ 2
     * _ _ y _ _ 3
     * _ _ _ _ _ 4
     */
    val board = Board.empty(5, 5).addWord("ray", (2, 1), Direction.Vertical) match {
      case Success(board) => board
      case Failure(exn) => 
        fail(s"Unexpected error: $exn")
        null
    }
    
    val rack = TileGroup.fromTraversable("abcd")
    
    val observer = new NotifyTester
    val builder = new TurnBuilder(board, rack)
    builder.attachObserver(observer.observe)
    
    // Do some changes. This just happens to be copied from testSuccessfulAddTiles
    val tiles = Map((1, 2) -> 'b', (3, 2) -> 'd')
    builder.addTiles(tiles) match {
      case Success(())   => ()
      case Failure(exn)  => fail(s"Unexpected exception: $exn")
    }
    
    // Reset, since it was just notified
    observer.wasNotified = false
    
    // Now, reload with the board above and ensure that no changes persist
    builder.reload(board, rack)
    
    assertTrue(observer.wasNotified)
    assertEquals(builder.getAdditions, Map())
    assertEquals(builder.getTiles, rack)
  }
  
  /**
   * Ensures that getBoard works, since I haven't used it anywhere else in these tests.
   */
  def testGetBoard {
    /*
     * 
     * 0 1 2 3 4
     * _ _ _ _ _ 0
     * _ _ r _ _ 1 
     * _ _ a _ _ 2
     * _ _ y _ _ 3
     * _ _ _ _ _ 4
     */
    val board = Board.empty(5, 5).addWord("ray", (2, 1), Direction.Vertical) match {
      case Success(board) => board
      case Failure(exn) => 
        fail(s"Unexpected error: $exn") 
        null 
    } 
    
    val rack = TileGroup.fromTraversable("abcd") 
    val builder = new TurnBuilder(board, rack)
    
    // Do some changes. This just happens to be copied from testSuccessfulAddTiles
    val tiles = Map((1, 2) -> 'b', (3, 2) -> 'd')
    builder.addTiles(tiles) match {
      case Success(())   => ()
      case Failure(exn)  => fail(s"Unexpected exception: $exn")
    }
    
    // Finally, get at the board and ensure that it reflects the changes
    /*
     * 0 1 2 3 4
     * _ _ _ _ _ 0
     * _ _ r _ _ 1 
     * _ b a d _ 2
     * _ _ y _ _ 3
     * _ _ _ _ _ 4
     */
    val newBoard = builder.getBoard
    assertEquals(newBoard(2, 1).get, 'r')
    assertEquals(newBoard(2, 2).get, 'a')
    assertEquals(newBoard(2, 3).get, 'y')
    assertEquals(newBoard(1, 2).get, 'b')
    assertEquals(newBoard(3, 2).get, 'd')
  }
  
  def testSuccessfulAddWord {
    /*
     * 0 1 2 3 4
     * _ _ _ _ _ 0
     * _ _ r _ _ 1 
     * _ _ a _ _ 2
     * _ _ y _ _ 3
     * _ _ _ _ _ 4
     */
    val board = Board.empty(5, 5).addWord("ray", (2, 1), Direction.Vertical) match {
      case Success(board) => board
      case Failure(exn) => 
        fail(s"Unexpected error: $exn") 
        null 
    } 
    
    val rack = TileGroup.fromTraversable("abcd") 
    val observer = new NotifyTester
    val builder = new TurnBuilder(board, rack)
    builder.attachObserver(observer.observe)
    
    builder.addWord("bad", (1, 2), Direction.Horizontal) match {
      case Success(_)   => ()
      case Failure(exn) => fail(s"Unexpected error $exn")
    }
    
    assertTrue(observer.wasNotified)
    assertEquals(builder.getAdditions, Map((1, 2) -> 'b', (3, 2) -> 'd'))
    assertEquals(builder.getTiles, TileGroup.fromTraversable(List('a', 'c')))
  }
  
  /**
   * Ensure that words which extend off the board fails.
   */
  def testNotOnBoardAddWord {
    val board = Board.empty(5, 5)
    val rack = TileGroup.fromTraversable("abcdef")
    
    val observer = new NotifyTester
    val builder = new TurnBuilder(board, rack)
    builder.attachObserver(observer.observe)
    
    builder.addWord("fade", (2, 2), Direction.Horizontal) match {
      case Success(_)   => fail(s"Expected NotOnBoardError")
      case Failure(err: NotOnBoardError) =>
        assertEquals(err.col, 5)
        assertEquals(err.row, 2)
      case Failure(err) => fail(s"Expected NotOnBoardError, got $err")
    }
    
    // Ensure that nothing propagated to the builder's board
    assertFalse(observer.wasNotified)
    assertEquals(builder.getAdditions, Map())
    assertEquals(builder.getTiles, rack)
  }
  
  /**
   * Ensure that words which require word not in the rack fail.
   */
  def testNotOnRackAddWord {
    val board = Board.empty(5, 5)
    val rack = TileGroup.fromTraversable("abcdef")
    
    val observer = new NotifyTester
    val builder = new TurnBuilder(board, rack)
    builder.attachObserver(observer.observe)
    
    builder.addWord("zed", (2, 2), Direction.Horizontal) match {
      case Success(_)   => fail(s"Expected NotInTileGroup")
      case Failure(err: NotInTileGroup) =>
        assertEquals(err.tiles, TileGroup.fromTraversable("z"))
      case Failure(err) => fail(s"Expected NotOnBoardError, got $err")
    }
    
    // Ensure that nothing propagated to the builder's board
    assertFalse(observer.wasNotified)
    assertEquals(builder.getAdditions, Map())
    assertEquals(builder.getTiles, rack)
  }
  
  /**
   * Ensure that words which conflict with the content already in the builder,
   * fails.
   */
  def testConflictAddWord {
    val board = Board.empty(5, 5)
    val rack = TileGroup.fromTraversable("abcdef")
    
    val observer = new NotifyTester
    val builder = new TurnBuilder(board, rack)
    builder.attachObserver(observer.observe)
    
    /* 0 1 2 3 4
     * _ _ _ _ _ 0
     * _ _ _ _ _ 1
     * _ _ f _ _ 2
     * _ _ e _ _ 3 
     * _ _ d _ _ 4
     */
    builder.addWord("fed", (2, 2), Direction.Vertical) match {
      case Success(_)   => ()
      case Failure(exn) => fail(s"Unexpected $exn")
    }
    
    observer.wasNotified = false
    
    /*
     * First, ensure that any tiles which conflict with tiles already in the 
     * builder, cause a failure. In this case, the "a" in cab conflicts with
     * the "e" in fed
     */
    builder.addWord("cab", (1, 3), Direction.Horizontal) match {
      case Success(_)   => fail(s"Expected PermanentTileError")
      case Failure(exn: PermanentTileError) =>
        assertEquals(exn.col, 2)
        assertEquals(exn.row, 3)
      case Failure(exn) => fail(s"Expected PermantentTileError, got $exn")
    }
    
    assertFalse(observer.wasNotified)
    assertEquals(builder.getAdditions, 
      Map((2, 2) -> 'f', (2, 3) -> 'e', (2, 4) -> 'd'))
    assertEquals(builder.getTiles, TileGroup.fromTraversable("abc"))
    
    /**
     * Now, ensure that conflicts with the underlying board also causes an
     * error.
     */
    val fedBoard = builder.getBoard
    val fedRack = builder.getTiles
    builder.reload(fedBoard, fedRack)
    observer.wasNotified = false
    
    builder.addWord("cab", (1, 3), Direction.Horizontal) match {
      case Success(_)   => fail(s"Expected DuplicateTilesError")
      case Failure(exn: DuplicateTilesError) =>
        assertEquals(exn.tiles, Set((2, 3)))
      case Failure(exn) => fail(s"Expected DuplicateTilesError, got $exn")
    }
    
    assertFalse(observer.wasNotified)
    assertEquals(builder.getAdditions, Map())
    assertEquals(builder.getTiles, TileGroup.fromTraversable("abc"))
  }
}