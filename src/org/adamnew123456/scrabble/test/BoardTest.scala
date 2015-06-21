package org.adamnew123456.scrabble.test

import scala.util.{Try, Success, Failure}

import org.adamnew123456.scrabble.{Board, DuplicateTilesError, NotOnBoardError, Direction, Word}

import org.junit.Test
import junit.framework.TestCase
import org.junit.Assert._

/**
 * Ensures that the Board works as intended
 */
class BoardTest extends TestCase {
  var board: Board = _
  
  override def setUp() {
    board = new Board(Map((3, 3) -> 'h', (4, 3) -> 'i'), 7, 7)
  }
  
  /**
   * Ensures that the board retrieval function works properly.
   */
  def testRetrieval() {
    assertEquals(board(3, 3), Some('h'))
  }
  
  /**
   * Ensure that proper insertion of characters onto the board works correctly
   */
  def testCorrectInsertion() {
    board.addCharacters(Map((3, 4) -> 'a', (3, 5) -> 't')) match {
      case Success(newBoard) => board = newBoard
      case Failure(exn) => fail(s"Exception: $exn")
    }
  }
  
  /**
   * Ensures that failures raise the proper exceptions:
   * 
   * - Inserting a character off the board: NotOnBoardError
   * - Inserting on top of existing character: DuplicateTilesError
   */
  def testIncorrectInsertion() {
    board.addCharacters(Map((-1, -1) -> 'x')) match {
      case Success(_) => fail("Expected to see a NotOnBoardError")
      case Failure(_: NotOnBoardError) => ()
      case Failure(exn) => fail(s"Expected NotOnBoardError, got $exn")
    }
    
    board.addCharacters(Map((3, 3) -> 'x')) match {
      case Success(_) => fail("Expected to see a DuplicateTilesError")
      case Failure(_: DuplicateTilesError) => ()
      case Failure(exn) => fail(s"Expected DuplicateTilesError, got $exn")
    }
  }
  
  /**
   * Ensures that words are computed properly by the board.
   */
  def testWords() {
    /*
     * Currently the board looks like this:
     * 
     * _ _ _ _ _ _ _
     * _ _ _ _ _ _ _
     * _ _ _ _ _ _ _
     * _ _ _ h i _ _
     * _ _ _ _ _ _ _
     * _ _ _ _ _ _ _
     * _ _ _ _ _ _ _
     * 
     * 
     * We want to make it look like this:
     * 
     *   0 1 2 3 4 5 6
     * 0 _ _ _ _ _ _ _
     * 1 _ _ _ c o t _
     * 2 _ _ _ _ _ a _
     * 3 _ _ s h i p _
     * 4 _ _ _ _ _ p _
     * 5 _ _ _ _ r e d
     * 6 _ _ _ _ _ d o
     * 
     * This should report the words:
     * 
     * - (3, 1) cot
     * - (2, 3) ship
     * - (3, 5) red
     * - (5, 6) do
     * - (6, 5) do
     * - (5, 1) tapped
     */
    
     board.addCharacters(
         Map((2, 3) -> 's',
             (3, 1) -> 'c',
             (4, 1) -> 'o',
             (4, 5) -> 'r',
             (5, 1) -> 't',
             (5, 2) -> 'a',
             (5, 3) -> 'p',
             (5, 4) -> 'p',
             (5, 5) -> 'e',
             (5, 6) -> 'd',
             (6, 5) -> 'd',
             (6, 6) -> 'o'
         )) match {
       case Success(newBoard) => board = newBoard
       case Failure(exn) => fail(s"Unexpected exception: $exn")
     }
         
     val words = board.findWords
     val expectedWords = Set(
         Word("cot", Direction.Horizontal, (3, 1)),
         Word("ship", Direction.Horizontal, (2, 3)),
         Word("red", Direction.Horizontal, (4, 5)),
         Word("do", Direction.Horizontal, (5, 6)),
         Word("tapped", Direction.Vertical, (5, 1)),
         Word("do", Direction.Vertical, (6, 5))
         )
         
     assertEquals(words, expectedWords)
  }
}