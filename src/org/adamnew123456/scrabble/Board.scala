package org.adamnew123456.scrabble

import scala.collection.mutable.{MutableList, HashSet, Queue, HashMap}
import scala.util.{Try, Success, Failure}


/**
 * An Exception which is thrown when somebody attempts to add characters to a
 * board which already has characters in the same location.
 */
case class DuplicateTilesError(tiles: Set[(Int, Int)]) extends Exception {
  override def toString = s"DuplicateTilesError(${tiles.mkString("; ")})"
}

/**
 * An Exception which is thrown when an invalid Board coordinate is received.
 */
case class NotOnBoardError(col: Int, row: Int) extends Exception {
  override def toString = s"NotOnBoardError($col, $row)"
}

/**
 * A direction on the board - vertical means top-down, while horizontal means
 * left-right.
 */
object Direction extends Enumeration {
  type Type = Value
  val Vertical, Horizontal = Value
}

/**
 * A single word on the board, which spans a range between two squares.
 */
case class Word(text: String, orientation: Direction.Type, start: (Int, Int))

/**
 * The Scrabble board is a two-dimensional matrix, which contains the 
 * characters on the board.
 * 
 * In addition, the board can assemble a list of words currently on
 * the board - note that these words are not guaranteed to be valid,
 * they have to be checked.
 */
class Board(board: Map[(Int, Int), Char], width: Int, height: Int) {
  /**
   * Gets the character on the board at the given location, or None.
   */
  def apply(column: Int, row: Int): Option[Char] = board.get((column, row))
  
  /**
   * Adds characters to the board, producing a new Board.
   * 
   * Note that this will fail if characters in the input share the same
   * location as they do in the board (that is, the caller attempts to
   * put two tiles on the same square).
   */
  def addCharacters(characters: Map[(Int, Int), Char]): Try[Board] = {
    val overlappingCharacters = characters.keySet.intersect(board.keySet)
    if (!overlappingCharacters.isEmpty) {
      Failure(DuplicateTilesError(overlappingCharacters))
    } else {
      // Ensure that all of the characters we're adding are valid coordinates
      // for this particular board
      characters.foreach { case ((col, row), _) =>
        if (row < 0 || row >= width) {
          return Failure(NotOnBoardError(col, row))
        } else if (col < 0 || col >= height) {
          return Failure(NotOnBoardError(col, row))
        }
      }
      
      Success(new Board(board ++ characters, width, height))
    }
  }
  
  /**
   * Adds a word to a given position on the board.
   * 
   * Note that this will happily put a tile onto another tile, as long as they
   * are the same. It will only raise an error if the tile on the board differs
   * from its corresponding tile in the input, or if the word is too large to
   * fit on the board.
   * 
   * For example, take this board:
   * 
   *   a b c d e
   * 1 _ _ _ _ _
   * 2 _ _ _ _ _
   * 3 r a t _ _
   * 4 _ _ _ _ _
   * 5 _ _ _ _ _
   * 
   * If we want to add the word "star" starting at c2 and going down, then
   * this method would be called as:
   * 
   *   addWord("star", (2, 1), Direction.Vertical)
   */
  def addWord(word: String, location: (Int, Int), direction: Direction.Type): Try[Board] = {
    // This moves the location ahead for each character in the word
    def ahead(loc: (Int, Int)) = (loc, direction) match {
      case ((col, row), Direction.Vertical) => (col, row + 1)
      case ((col, row), Direction.Horizontal) => (col + 1, row)
    }
    
    // Figure out if the word is too long to fit on the board
    val endOfWord = (location, direction) match {
      case ((col, row), Direction.Vertical) => (col, row + (word.length - 1))
      case ((col, row), Direction.Horizontal) => (col + (word.length - 1), row)
    }
    
    if (endOfWord._1 >= width || endOfWord._1 < 0 ||
        endOfWord._2 >= height || endOfWord._2 < 0) {
      Failure(NotOnBoardError(endOfWord._1, endOfWord._2))
    } else {
      val newBoard = new HashMap[(Int, Int), Char]()
      newBoard ++= board
      
      // Go through each character in the word, and figure out if it overlaps
      // with a tile on the board, that is not the same as the character.
      word.toList.foldLeft(location) { 
        (position: (Int, Int), letter: Char) =>
          if (board.contains(position) && board(position) != letter) {
            return Failure(DuplicateTilesError(Set(position)))
          } else {
            newBoard(position) = letter
            ahead(position)
          }
      }
      
      Success(new Board(newBoard.toMap, width, height))
    }
  }
  
  /**
   * This finds all the words on the board.
   * 
   * First, it starts from the middle tile, traverses the following way:
   * 
   * - If on a horizontal word, it goes as far left as it can, and
   *   accumulates the word's characters. If, in doing so, it sees any vertical
   *   tiles, it adds those to the queue of locations to visit.
   *  - If on a vertical word, it goes as far up as it can, and
   *   accumulates the word's characters. If, in doing so, it sees any horizontal
   *   tiles, it adds those of the queue of locations to visit.
   */
  def findWords: Set[Word] = {
    val words = HashSet[Word]()
    val visited = HashSet[(Int, Int)]()
    val toVisit = Queue[(Int, Int, Direction.Type)]()
    
    /*
     *  Start from the center square, and work from there. Note that the center
     *  can have any orientation, so we have to figure out if it is horizontal or
     *  vertical.
     */
    val (centerRow, centerCol) = (width / 2, height / 2)
    val centerUp = (centerCol, centerRow - 1)
    val centerDown = (centerCol, centerRow + 1)
    val centerLeft = (centerCol - 1, centerRow)
    val centerRight = (centerCol + 1, centerRow)
    
    if (board.contains(centerLeft) || board.contains(centerRight)) {
      toVisit += ((centerCol, centerRow, Direction.Horizontal))
    } else {
      toVisit += ((centerCol, centerRow, Direction.Vertical))
    }
    
    // Takes a sequence of tiles, and figures out where the word boundaries are
    def getWordTiles(beforeTiles: Seq[(Int, Int)], afterTiles: Seq[(Int, Int)]) = {
      val beforeWordTiles = beforeTiles.takeWhile(board.contains(_))
      val afterWordTiles = afterTiles.takeWhile(board.contains(_))
      
      beforeWordTiles.reverse ++ afterWordTiles
    }
    
    // Makes a sequence of tiles into a single word
    def makeWord(tiles: Seq[(Int, Int)]) = tiles.map(board(_)).mkString("")
    
    /*
     * Gets the searchable tiles from a sequence of tiles (those that have letters
     * on them, and haven't been searched yet)
     */
    def searchableTiles(tiles: Seq[(Int, Int)]) =
      tiles.filter(tile => board.contains(tile) && !visited.contains(tile))
      
    /*
     *  Adds a direction to a sequence of coordinates, suitable for adding to
     *  the toVisit list
     */
    def addDirection(seq: Seq[(Int, Int)], direction: Direction.Type) =
      seq.map {case (col, row) => (col, row, direction)}
    
    def visitHorizontal(location: (Int, Int)) {
      val (col, row) = location
      
      /*
       *     blank space
       *     v
       *     _ c a r r o t _ 
       *           ^
       *            start here
       *         
       *     _ c a r r o t _ 
       *       |-<-<
       *           find our way to the c, which is the leftmost we can get
       *           before we hit a blank
       *     
       *     _ c a r r o t _
       *           ^-- start here again
       *         
       *     _ c a r r o t _
       *           >->->-> 
       *           similar process to that in the second step, but work our
       *           way right until we hit a blank
       */
      val leftTiles = (col - 1).to(0).by(-1).map((_, row))
      val rightTiles = col.to(width - 1).map((_, row))
      
      val wordTiles = getWordTiles(leftTiles, rightTiles)
      words += Word(makeWord(wordTiles), Direction.Horizontal, wordTiles.head)
      visited ++= wordTiles
      
      /* 
       * Last, get all the tiles above and below the word. If any of them
       * are not yet visited, and have letters on them, queue them up for
       * future searching.
       */
      val topTiles = wordTiles.map {case (col, row) => (col, row - 1)}
      toVisit ++= addDirection(searchableTiles(topTiles), Direction.Vertical)
      
      val bottomTiles = wordTiles.map { case (col, row) => (col, row + 1)}
      toVisit ++= addDirection(searchableTiles(bottomTiles), Direction.Vertical)
    }
    
    def visitVertical(location: (Int, Int)) {
      val (col, row) = location
      
      val aboveTiles = (row - 1).to(0).by(-1).map((col, _))
      val belowTiles = row.to(height - 1).map((col, _))
      
      val wordTiles = getWordTiles(aboveTiles, belowTiles)
      words += Word(makeWord(wordTiles), Direction.Vertical, wordTiles.head)
      visited ++= wordTiles
      
      val leftTiles = wordTiles.map {case (col, row) => (col - 1, row)}
      toVisit ++= addDirection(searchableTiles(leftTiles), Direction.Horizontal)
      
      val rightTiles = wordTiles.map { case (col, row) => (col + 1, row)}
      toVisit ++= addDirection(searchableTiles(rightTiles), Direction.Horizontal)
    }
    
    while (!toVisit.isEmpty) {
      toVisit.dequeue match {
        case (col, row, Direction.Horizontal) => visitHorizontal((col, row))
        case (col, row, Direction.Vertical) => visitVertical((col, row))
      }
    }
    
    words.toSet
  }
}