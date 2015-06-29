package org.adamnew123456.scrabble.players

import java.io.EOFException
import scala.annotation.tailrec

import scala.collection.mutable.HashMap
import scala.io.{AnsiColor, Source}

import org.adamnew123456.scrabble.{BasePlayer, Board, Config}
import org.adamnew123456.scrabble.players.TerminalColorScheme._

/**
 * This is a player which interacts via a terminal, using basic 
 */
class TerminalPlayer(name: String, game: Config) extends BasePlayer(name, game) {
  var previousScore = 0
  val inputStream = Source.stdin.getLines
  
  // The color scheme used for printing
  
  /**
   * Pads a string on the left
   */
  private def leftJustify(text: String, length: Int, elem: Char = ' '): String =
    text.reverse.padTo(length, elem).reverse
  
  /**
   * Prints out a Board using ANSI color sequences.
   */
  def printColoredBoard(board: Board) {
    
    // Column numbers appear on top of the first row
    val colNumbers = 1.to(board.width)
    // Row numbers appear at the beginning of each row
    val rowNumbers = 1.to(board.height)
    
    // Figure out how much padding will be necessary to align up all the
    // numbers and tiles equally
    val colNumberPadding = colNumbers.map(_.toString.length).max
    val rowNumberPadding = rowNumbers.map(_.toString.length).max
    
    // Pad the first line in accordance with the row numbers (plus an extra,
    // to include the space after the row numbers)
    val leftPadding = List.fill(' ')(rowNumberPadding + 1).mkString("")
    println(leftPadding + colNumbers.map {col: Int => 
      leftJustify(col.toString, colNumberPadding)
    })
    
    for (row <- 0.to(board.height - 1)) {
      val line = for (col <- 0.to(board.width - 1)) 
        yield {
          board(col, row) match {
            case Some(tile) => s"${BoardTile}$tile${AnsiColor.RESET}"
            case None       => s"${BoardEmpty}_${AnsiColor.RESET}"
          }
        }
      
      val justifiedLineNumber = leftJustify(rowNumbers(row).toString, rowNumberPadding)
      val colJustifiedLine = line.map(leftJustify(_, colNumberPadding))
      println(s"${justifiedLineNumber} ${colJustifiedLine.mkString(" ")}")
    }
  }
  
  /**
   * Reads a line from the user, or throws EOFException if it cannot.
   */
  private def getInput(prompt: String): String = {
    print(prompt)
    if (inputStream.hasNext) {
      inputStream.next
    } else {
      throw new EOFException()
    }
  }
  
  def startTurn(board: Board, tiles: List[Char], scores: Map[String, Int]) {
    printColoredBoard(board)
    for ((name, score) <- scores)
      
      println(s"Score $name: ${Score}$score${AnsiColor.RESET}")
      
    
    val tileRack = tiles.mkString(" ")
    println(s"Tiles: ${RackTile}$tileRack${AnsiColor.RESET}")
    
    previousScore = scores(name)
  }
  
  def endTurn(board: Board, tiles: List[Char], scores: Map[String, Int]) {
    val scoreDifference = scores(name) - previousScore
    println(s"You scored ${Score}$scoreDifference${AnsiColor.RESET} this turn")
  }
  
  
  def replaceTiles(tiles: List[Char]): List[Char] = {
    val unsanatizedInputTiles = getInput(s"Tiles to replace:")
    unsanatizedInputTiles.toList
      .filter(!game.letterDistribution.keySet.contains(_))
  }
  
  def turn(board: Board, tiles: List[Char]): Map[(Int, Int), Char] = {
    val processor = new TerminalCommandProcessor(this, board, game, tiles)
    
    @tailrec
    def runCommand: Map[(Int, Int), Char] = {
      processor.dispatchCommand(getInput("> ")) match {
        case Some(result) => result
        case None         => runCommand
      }
    }
    
    runCommand
  }
}