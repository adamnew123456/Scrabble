package org.adamnew123456.scrabble.players

import scala.collection.mutable.HashMap
import scala.io.AnsiColor
import scala.util.{Success, Failure}

import org.adamnew123456.scrabble.{BasePlayer, Board, Config, Direction, Word, WordScorer}
import org.adamnew123456.scrabble.players.TerminalColorScheme._

/**
 * This defines a terminal command - its name, arguments, and help text.
 */
case class TerminalCommand(name: String, args: List[String], help: String, fn: List[String] => Unit)

/**
 * This is responsible for processing commands on the terminal.
 * 
 * This supports the following commands:
 * 
 *   Add a word:          w ROW COL [H|V] WORD
 *   Remove a tile:       x ROW COL
 *   List tiles on rack:  r
 *   Show points added:   p
 *   Shows all tiles:     t
 *   Show current board:  b
 *   Submit to game:      s
 *   Help:                h
 */
class TerminalCommandProcessor(player: TerminalPlayer, board: Board, 
                               game: Config, scorer: WordScorer, 
                               tiles: List[Char]) {
  var isDone = false
  
  val boardAdditions = new HashMap[(Int, Int), Char]
  val tileCounts = new HashMap[Char, Int]
  
  // Record all the tiles, by number
  tiles.foreach {tile: Char => 
    if (!tileCounts.contains(tile)) {
      tileCounts(tile) = 0
    }
    
    tileCounts(tile) += 1
  }
  
  /**
   * A safer version of toInt, which wraps exceptions in Option[T].
   */
  private def toInt(x: String): Option[Int] = {
    try {
      Some(x.toInt)
    } catch {
      case e: NumberFormatException => None
    }
  }
  
  /**
   * These are utility functions which are used by the TerminalCommands defined
   * later. They all stick to the Either[L, R] type because they are meant to
   * be used monadically.
   */
  
  // Converts a pair of strings to a pair of coordinates, or an error message
  private def getCoordinate(rowText: String, columnText: String): Either[(Int, Int), String] = {
    val row = toInt(rowText) match {
      case Some(row) => row - 1 // The -1 is because the user sees the grid 
                                // as 1-indexed, not 0-indexed
      case None      => return Right(s"${rowText} is not a number")
    }
    
    if (row < 0 || row >= board.height) {
      return Right(s"${rowText} is not a valid row")
    }
    
    val col = toInt(columnText) match {
      case Some(col) => col - 1
      case None      => return Right(s"${columnText} is not a number")
    }
    
    if (col < 0 || col >= board.width) {
      return Right(s"${columnText} is not a valid col")
    }
    
    Left((row, col))
  }
  
  // Removes the given tiles from the player's rack
  private def removeWordFromRack(word: String): Either[Unit, String] = {
    // Count up each of the tiles that are present in the word
    val wordCounts = new HashMap[Char, Int]
    word.foreach { char: Char =>
      if (!wordCounts.contains(char)) {
        wordCounts(char) = 0
      }
      wordCounts(char) += 1
    }
    
    // Ensure that all of the characters in the word count can be drawn from
    // the player's rack
    val invalidTiles = wordCounts.filter {
      case (char, count) => !tileCounts.contains(char) || tileCounts(char) < count
    }
    
    if (invalidTiles.isEmpty) {
      // Remove the tiles, since we know this won't end up putting the rack in
      // an invalid state
      wordCounts.foreach { case (tile, count) =>
        tileCounts(tile) -= count
      }
      
      Left(())
    } else {
      val coloredTiles = invalidTiles.keys.map {
        tile: Char => s"${RackTile}$tile${AnsiColor.RESET}"
      }
      
      Right(s"You don't have enough of the following tiles: ${coloredTiles.mkString(", ")}")
    }
  }
  
  // Adds the given word to the temporary board
  private def addToBoard(
      location: (Int, Int), 
      direction: Direction.Type, 
      word: String): Either[Unit, String] = {
    // Note: Part of adding the word to the board is removing the necessary
    // tiles from the player's rack. For this reason, we have to keep a running
    // total of what tiles still remain.
    //
    // The tile counts are copied because, if it turns out this word can't be
    // added, we don't want to trash the good tile state. Same with the board.
    val tempTileCounts = tileCounts.clone
    val tempBoardAdditions = boardAdditions.clone
    
    // The location here is not in the standard (column, row) because the user
    // enters it as (row, colum)
    var (startRow, startCol) = location
    
    val tiles = (if (direction == Direction.Horizontal) {
      for (col <- startCol.to(startCol + word.length)) 
        yield (col, startRow)
    } else {
      for (row <- startRow.to(startRow + word.length))
        yield (row, startCol)
    }).toList
    
    // If the final tile in the sequence is off the board, then the word
    // is too long
    val (lastCol, lastRow) = tiles.last
    if (lastCol >= board.width || lastRow >= board.height) {
      return Right(s"'${word}' is too long to fit on the board")
    }
    
    tiles.zip(word).foreach {
      case ((col, row), tile) =>
        // If the tile overlaps with the board, and it isn't the same, then 
        var removeTileFromRack = true
        
        board(col, row) match {
          case Some(boardTile) =>
            if (boardTile != tile) {
              return Right(s"Tile at ${row + 1} ${col + 1} isn't ${tile}")
            } else {
              removeTileFromRack = false
            }
          case None => ()
        }
        
        boardAdditions.get((col, row)) match {
          case Some(boardTile) =>
            if (boardTile != tile) {
              return Right(s"Tile at ${row + 1} ${col + 1} isn't ${tile}")
            } else {
              removeTileFromRack = false
            }
          case None => ()
        }
        
        // Ensure that the tile is actually in the player's rack, if we
        // really have to use it
        if (removeTileFromRack) {
          if (tempTileCounts(tile) == 0) {
            return Right(s"Not enough ${RackTile}$tile{AnsiColor.RESET} tiles.")
          } else {
            tempTileCounts(tile) -= 1
          }
        }
        
        // Add it to the board
        tempBoardAdditions((col, row)) = tile
    }
    
    // If we've gotten this far, then we can update the board and the rack
    boardAdditions ++= tempBoardAdditions
    tileCounts ++= tempTileCounts
    
    Left(())
  }
  
  val commands: List[TerminalCommand] = List(
    TerminalCommand("w", List("row", "column", "orientation (h|v)", "word"),
                    "Adds a new word to the board.",
                    {args: List[String] =>
                      val (rowArg, colArg, orientArg, wordArg) = args match {
                        case row :: col :: orient :: word :: Nil => 
                          (row, col, orient.toLowerCase, word.toLowerCase)
                          
                        // Ignore other cases, since the arg list has been
                        // checked for us
                        case _ => throw new MatchError
                      }
                      
                      val result = for {
                        rowCol <- getCoordinate(rowArg, colArg).left
                        orientation <- (orientArg match {
                          case "h" => Left(Direction.Horizontal)
                          case "v" => Left(Direction.Vertical)
                          case _   => Right(s"Orientation must be 'h' or 'v'")
                        }).left
                        
                        boardCheck <- addToBoard(rowCol, orientation, wordArg).left
                      } yield boardCheck
                      
                      result match {
                        case Left(_)    => ()
                        case Right(err) => println(err)
                      }
                    }),
    
    TerminalCommand("x", List("row", "column"),
                    "Removes a tile from the board.",
                    {args: List[String] =>
                      val (rowArg, colArg) = args match {
                        case row :: col :: Nil =>
                          (row, col)
                          
                        // Ignore other cases, since the arg list has been
                        // checked for us
                        case _ => throw new MatchError
                      }
                      
                      val result = for {
                        rowCol <- getCoordinate(rowArg, colArg).left
                        boardCheck <- (rowCol match {
                          case (row, col) =>
                            if (board(col, row).isDefined) {
                              Right(s"${row + 1}, ${col + 1} cannot be removed")
                            } else if (!boardAdditions.contains((col, row))) {
                              Right(s"${row + 1}, ${col + 1} is empty")
                            } else {
                              val tile = boardAdditions((col, row))
                              tileCounts(tile) += 1
                              boardAdditions.remove((col, row))
                              
                              Left()
                            }
                        }).left
                      } yield boardCheck
                      
                      result match {
                        case Left(_)    => ()
                        case Right(err) => println(err)
                      }
                    }),
                    
    TerminalCommand("r", Nil, "Lists the tiles on your rack",
                    {args: List[String] =>
                      // Expand the from a (tile -> count) map to a list of tiles
                      val tiles = tileCounts.flatMap {case (tile, count) => List.fill(count)(tile)}
                      tiles.foreach { tile: Char =>
                        print(s"${RackTile}$tile${AnsiColor.RESET} ")
                      }
                      println()
                    }),
                    
    TerminalCommand("p", Nil, "Shows points added this turn",
                    {args: List[String] =>
                      val oldWords = board.findWords.map(_.text)
                      
                      val points = board.addCharacters(boardAdditions.toMap) match {
                        case Success(newBoard) =>
                          val newWords = newBoard.findWords.map(_.text)
                          val addedWords = scorer.computeModifiedWords(oldWords, newWords)
                          
                          scorer.computeTurnScore(addedWords) match {
                            case Success(points) => points
                            case Failure(exn) => 
                              println(s"Error: $exn")
                              0
                          }
                        case Failure(exn) =>
                          println(s"Error: $exn")
                          0
                      }
                      
                      println(s"Added score: ${Score}$points${AnsiColor.RESET}")
                    }),
                    
    TerminalCommand("t", Nil, "Shows a list all tiles and their point values",
                    {args: List[String] =>
                      // Show five letter-score pairs per row, to avoid taking
                      // too many lines
                      game.letterScores.toList.grouped(5).foreach { row: List[(Char, Int)] =>
                        row.foreach { case (letter, score) => 
                          print(s"${RackTile}$letter${AnsiColor.RESET} ${Score}$score${AnsiColor.RESET}")
                          print(" | ")
                        }
                        println()
                      }
                    }),
                    
    TerminalCommand("b", Nil, "Shows the current content of the board",
                    {args: List[String] =>
                      println("----- Old Board")
                      player.printColoredBoard(board)
                      
                      val newBoard = board.addCharacters(boardAdditions.toMap) match {
                        case Success(newBoard) =>
                          println("----- Current Board")
                          player.printColoredBoard(newBoard)
                        case Failure(exn) =>
                          println(s"Error: $exn")
                      }
                    }),
                    
    TerminalCommand("s", Nil, "Submit this turn",
                    {args: List[String] =>
                      isDone = true
                    }),
                    
    TerminalCommand("h", Nil, "Show a help menu",
                    {args: List[String] =>
                      commands.foreach {cmd: TerminalCommand =>
                        println(s"${cmd.name} ${cmd.args.mkString(" ")}\n    ${cmd.help}")
                      }
                    })
  )
  
  val commandTuples = commands.map {cmd: TerminalCommand => (cmd.name, cmd)}
  val commandMap = Map[String, TerminalCommand](commandTuples:_*)
  
  /**
   * Parses out a command line, and dispatches to the proper function.
   */
  def dispatchCommand(commandLine: String): Option[Map[(Int, Int), Char]] = {
    val (command, args) = commandLine.trim.split("\\s+").toList match {
      case cmd :: args => (cmd, args)
      case _           => return None
    }
    
    commandMap.get(command) match {
      case Some(cmd) =>
        val argSpec = cmd.args
        if (argSpec.length == cmd.args.length) {
          cmd.fn(args)
        }
      case None =>
        commandMap("h").fn(Nil)
    }
    
    if (isDone) {
      Some(boardAdditions.toMap)
    } else {
      None
    }
  }
}