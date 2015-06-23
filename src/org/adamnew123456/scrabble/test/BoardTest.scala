package org.adamnew123456.scrabble.test

import scala.util.{ Try, Success, Failure }

import org.adamnew123456.scrabble.{ Board, DuplicateTilesError, NotOnBoardError, Direction, Word }

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
      case Success(newBoard) =>
        assertEquals(newBoard(3, 4).get, 'a')
        assertEquals(newBoard(3, 5).get, 't')
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
      case Success(_)                  => fail("Expected to see a NotOnBoardError")
      case Failure(_: NotOnBoardError) => ()
      case Failure(exn)                => fail(s"Expected NotOnBoardError, got $exn")
    }

    board.addCharacters(Map((3, 3) -> 'x')) match {
      case Success(_)                      => fail("Expected to see a DuplicateTilesError")
      case Failure(_: DuplicateTilesError) => ()
      case Failure(exn)                    => fail(s"Expected DuplicateTilesError, got $exn")
    }
  }

  /**
   * Ensures that adding a word to the board works.
   */
  def testAddWord() {
    board.addWord("pit", (4, 2), Direction.Vertical) match {
      case Success(newBoard) =>
        assertEquals(newBoard(4, 2).get, 'p')
        assertEquals(newBoard(4, 3).get, 'i')
        assertEquals(newBoard(4, 4).get, 't')
      case Failure(exn) => fail(s"Unexpected exception: $exn")
    }

    board.addWord("hips", (3, 3), Direction.Horizontal) match {
      case Success(newBoard) =>
        assertEquals(newBoard(3, 3).get, 'h')
        assertEquals(newBoard(4, 3).get, 'i')
        assertEquals(newBoard(5, 3).get, 'p')
        assertEquals(newBoard(6, 3).get, 's')
      case Failure(exn) => fail(s"Unexpected exception: $exn")
    }
  }

  /**
   * Ensure that adding words incorrectly fails in the expected ways.
   */
  def testInvalidAddWord() {
    // Overwrite a tile with a different tile
    board.addWord("sit", (3, 3), Direction.Horizontal) match {
      case Success(_) =>
        fail(s"Expected DuplicateTilesError")
      case Failure(_: DuplicateTilesError) => ()
      case Failure(exn) =>
        fail(s"Expected DuplicateTilesError, got $exn")
    }
    
    // Add a word which is too wide
    board.addWord("supercalifragilistic", (0, 0), Direction.Horizontal) match {
      case Success(_) => 
        fail(s"Expected NoOnBoardError")
      case Failure(_ : NotOnBoardError) => ()
      case Failure(exn) =>
        fail(s"Expected NotOnBoarderror, got $exn")
    }
  }

  /**
   * Ensures that words are computed properly by the board.
   */
  def testWords() {
    /*
     * Currently the board looks like this:
     * 
     *   0 1 2 3 4 5 6
     * 0 _ _ _ _ _ _ _
     * 1 _ _ _ _ _ _ _
     * 2 _ _ _ _ _ _ _
     * 3 _ _ _ h i _ _
     * 4 _ _ _ _ _ _ _
     * 5 _ _ _ _ _ _ _
     * 6 _ _ _ _ _ _ _
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
    val wordsToAdd = Map(
      (3, 1, Direction.Horizontal) -> "cot",
      (2, 3, Direction.Horizontal) -> "ship",
      (4, 5, Direction.Horizontal) -> "red",
      (5, 6, Direction.Horizontal) -> "do",
      (5, 1, Direction.Vertical) -> "tapped",
      (6, 5, Direction.Vertical) -> "do")

    board = wordsToAdd.foldLeft(board) {
      (oldBoard: Board, posDirWord: ((Int, Int, Direction.Type), String)) =>
        val ((col, row, dir), word) = posDirWord
        oldBoard.addWord(word, (col, row), dir) match {
          case Success(newBoard) => newBoard
          case Failure(exn) =>
            fail(s"Unexpected exception: $exn")
            // We can't hit this, but the typechecker doesn't know that
            null
        }
    }

    val words = board.findWords
    val expectedWords = Set(
      Word("cot", Direction.Horizontal, (3, 1)),
      Word("ship", Direction.Horizontal, (2, 3)),
      Word("red", Direction.Horizontal, (4, 5)),
      Word("do", Direction.Horizontal, (5, 6)),
      Word("tapped", Direction.Vertical, (5, 1)),
      Word("do", Direction.Vertical, (6, 5)))

    assertEquals(words, expectedWords)
  }
}