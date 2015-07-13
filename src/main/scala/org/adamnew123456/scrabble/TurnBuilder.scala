package org.adamnew123456.scrabble

import scala.collection.mutable.{ HashMap, HashSet }
import scala.util.{ Try, Success, Failure }

/**
 * An exception for when the player tries to remove a tile that is not one they
 * placed down.
 */
case class PermanentTileError(col: Int, row: Int) extends Throwable

/**
 * This class allows a player to build a Board by adding and removing tiles,
 * while keeping the player's associated tile rack in sync with what happens
 * on the board.
 */
class TurnBuilder(private var board: Board, private var rack: TileGroup) {
  private val observers = new HashSet[TurnBuilder => Unit]
  private val boardAdditions = new HashMap[(Int, Int), Char]
  private var currentBoard = board
  private var currentRack = rack
  
  /**
   * Converts the internal representation of the game board into an actual
   * Board.
   */
  def getBoard: Board =
    board.addCharacters(boardAdditions.toMap) match {
      case Success(newBoard) => newBoard
      
      // This shouldn't happen, because we check every addition to the board -
      // if somebody calling addTiles requested an invalid move, we would have
      // rejected it
      case _                 => throw new MatchError
    }
  
  /**
   * Converts the internal representation of the player's remaining tiles 
   * into an actual TileGroup
   */
  def getTiles: TileGroup = currentRack
  
  /**
   * Gets the additions made to the Board via this TurnBuilder.
   */
  def getAdditions: Map[(Int, Int), Char] =
    boardAdditions.toMap
  
  /**
   * Adds tiles to the TurnBuilder's board, removes them from the player's 
   * rack, and and updates all observers.
   */
  def addTiles(tiles: Map[(Int, Int), Char]): Try[Unit] = {
    val tilesUsed = TileGroup.fromTraversable(tiles.values)
    
    val rack = for {
      // We're just using Board.addCharacters to do the checking - we don't 
      // care about the result
      tempBoard <- currentBoard.addCharacters(boardAdditions.toMap)
      _ <- tempBoard.addCharacters(tiles)
      
      newRack <- currentRack.remove(tilesUsed)
    } yield newRack
    
    rack match {
      case Success(rack) =>
        boardAdditions ++= tiles
        currentRack = rack
        
        notifyObservers
        Success(())
      case Failure(exn) =>
        Failure(exn)
    }
  }
  
  /**
   * Adds a word to the board. Note that this will automatically handle tiles
   * which are already on the board, and doesn't require them to be on the
   * rack.
   */
  def addWord(word: String, location: (Int, Int), direction: Direction.Type): Try[Unit] = {
    /*
     * Figure out what tiles are a part of the word.
     */
    val (startCol, startRow) = location
    
    val wordTiles = direction match {
      case Direction.Horizontal =>
        for (col <- startCol.to(startCol + word.length - 1))
          yield (col, startRow)
      case Direction.Vertical =>
        for (row <- startRow.to(startRow + word.length - 1))
          yield (startCol, row)
    }
    
    // Separate any tiles that conflict - that is, any tiles on the board which
    // don't correspond with the same character in the word
    val builtBoard = getBoard
    val tilesWord = wordTiles.zip(word)
    val conflicted = tilesWord.filter {
      case (loc, tile) =>
        builtBoard.get(loc) match {
          case Some(`tile`) => false
          case None         => false
          case _            => true
        }
    }
    
    conflicted.toList match {
      case ((col, row), _) :: _ => return Failure(PermanentTileError(col, row))
      case Nil                  => ()
    }
    
    // If there are no conflicts, then go through and add each character not 
    // already on the board
    val newTiles = tilesWord.filter {
      case (loc, tile) => !builtBoard.get(loc).isDefined
    }
    
    // Just try to add the tiles - since addTiles does checking before it does
    // anything, we can just rely on it for error handling
    val tilesToAdd = Map(newTiles: _*)
    addTiles(tilesToAdd)
  }
  
  /**
   * Removes tiles placed down this turn.
   */
  def removeTiles(spaces: Set[(Int, Int)]): Try[Unit] = {
    val offBoardTiles = spaces.filter {
      case (col, row) => col < 0 || col >= board.width || row < 0 || row >= board.height
    }.toList
      
    offBoardTiles match {
      case (col: Int, row: Int) :: _ =>
        Failure(NotOnBoardError(col, row))
      case Nil =>
        val permanentTiles = spaces.filter(!boardAdditions.contains(_)).toList
        permanentTiles match {
          case (col: Int, row: Int) :: _ =>
            Failure(PermanentTileError(col, row))
          case Nil =>
            val tiles = spaces.map(boardAdditions(_))
            boardAdditions --= spaces
            currentRack = currentRack.merge(TileGroup.fromTraversable(tiles))
            
            notifyObservers
            Success(())
        }
    }
  }
  
  /**
   * Discards all the current changes, and updates the current state from
   * the given board and tile rack. It also updates the observers.
   */
  def reload(newBoard: Board, newRack: TileGroup): Unit = {
    boardAdditions.clear
    currentBoard = newBoard
    currentRack = newRack
    notifyObservers
  }
  
  /**
   * Registers an observer function, which is called whenever the
   * board or rack changes.
   */
  def attachObserver(fn: TurnBuilder => Unit): Unit =
    observers += fn
    
  /**
   * Unregisters an observer function.
   */
  def detachObserver(fn: TurnBuilder => Unit): Unit =
    observers -= fn
    
  /**
   * Notifies all observers on a change.
   */
  def notifyObservers: Unit =
    observers.foreach(_(this))
}