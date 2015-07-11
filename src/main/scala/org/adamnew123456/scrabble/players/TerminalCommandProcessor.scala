package org.adamnew123456.scrabble.players

import scala.collection.mutable.HashMap
import scala.io.AnsiColor
import scala.util.{ Success, Failure }

import org.adamnew123456.scrabble._
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
class TerminalCommandProcessor(player: TerminalPlayer, turnBoard: Board,
                               game: Config, scorer: WordScorer,
                               turnTiles: TileGroup) {
  var isDone = false
  val turnBuilder = new TurnBuilder(turnBoard, turnTiles)

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

    if (row < 0 || row >= turnBoard.height) {
      return Right(s"${rowText} is not a valid row")
    }

    val col = toInt(columnText) match {
      case Some(col) => col - 1
      case None      => return Right(s"${columnText} is not a number")
    }

    if (col < 0 || col >= turnBoard.width) {
      return Right(s"${columnText} is not a valid col")
    }

    Left((col, row))
  }

  // Adds the given word to the temporary board
  private def addToBoard(
    location: (Int, Int),
    direction: Direction.Type,
    word: String): Either[Unit, String] = {
    // The location here is not in the standard (column, row) because the user
    // enters it as (row, colum)
    var (startRow, startCol) = location

    val wordSpaces = (if (direction == Direction.Horizontal) {
      for (col <- startCol.to(startCol + word.length - 1))
        yield (col, startRow)
    } else {
      for (row <- startRow.to(startRow + word.length - 1))
        yield (startCol, row)
    }).toList
    
    val wordTiles = Map(wordSpaces.zip(word): _*)
    
    // See if any parts of the word conflict with what is on the board
    val builderBoard = turnBuilder.getBoard
    val conflicting = wordTiles.filter {
      case ((col, row), tile) =>
        builderBoard(col, row) match {
          case Some(otherTile) =>
            otherTile != tile
          case None =>
            false
        }
    }
    
    if (!conflicting.isEmpty) {
      Right(s"Word conflicts with tiles already on board: $conflicting")
    } else {
      // If we don't conflict, then try to do add whatever tiles aren't on the
      // board
      val newTiles = wordTiles.filter {
        case ((col, row), tile) =>
          !builderBoard(col, row).isDefined
      }
      
      turnBuilder.addTiles(newTiles) match {
        case Success(()) => Left(())
        
        case Failure(DuplicateTilesError(spaces)) =>
          val outputSpaces = spaces.map { case (col, row) => (row + 1, col + 1)}
          Right(s"Error: The following tiles already exist - ${outputSpaces.mkString(", ")}")
          
        case Failure(NotInTileGroup(tiles)) =>
          val outputTiles = tiles.asList
          Right(s"Error: You lack the following tiles - ${outputTiles.mkString(", ")}")
          
        case Failure(exn) => Right(exn.toString)
      }
    }
  }

  val commands: List[TerminalCommand] = List(
    TerminalCommand("w", List("row", "column", "orientation (h|v)", "word"),
      "Adds a new word to the board.",
      { args: List[String] =>
        val (rowArg, colArg, orientArg, wordArg) = args match {
          case row :: col :: orient :: word :: Nil =>
            (row, col, orient.toLowerCase, word.toLowerCase)

          // Ignore other cases, since the arg list has been
          // checked for us
          case _ => throw new MatchError
        }

        val result = for {
          colRow <- getCoordinate(rowArg, colArg).left
          orientation <- (orientArg match {
            case "h" => Left(Direction.Horizontal)
            case "v" => Left(Direction.Vertical)
            case _   => Right(s"Orientation must be 'h' or 'v'")
          }).left

          boardCheck <- (turnBuilder.addWord(wordArg, colRow, orientation) match {
            case Success(())  => Left(())
            case Failure(exn) => Right(exn)
          }).left
        } yield boardCheck

        result match {
          case Left(_)    => ()
          case Right(err) => println(err)
        }
      }),

    TerminalCommand("x", List("row", "column"),
      "Removes a tile from the board.",
      { args: List[String] =>
        val (rowArg, colArg) = args match {
          case row :: col :: Nil =>
            (row, col)

          // Ignore other cases, since the arg list has been
          // checked for us
          case _ => throw new MatchError
        }

        val result = for {
          colRow <- getCoordinate(rowArg, colArg).left
          boardCheck <- (turnBuilder.removeTiles(Set(colRow)) match {
            case Success(()) => Left(())
            case Failure(PermanentTileError(col, row)) =>
              Right(s"Cannot remove at ${row + 1}, ${col + 1}")
            case Failure(err) =>
              Right(err.toString)
          }).left
        } yield boardCheck

        result match {
          case Left(_)    => ()
          case Right(err) => println(err)
        }
      }),

    TerminalCommand("r", Nil, "Lists the tiles on your rack",
      { args: List[String] =>
        turnBuilder.getTiles.asList.foreach { tile: Char =>
          print(s"${RackTile}$tile${AnsiColor.RESET} ")
        }
        println()
      }),

    TerminalCommand("p", Nil, "Shows points added this turn",
      { args: List[String] =>
        val oldWords = turnBoard.findWords

        val builderBoard = turnBuilder.getBoard
        val newWords = builderBoard.findWords
        val addedWords = scorer.computeModifiedWords(oldWords, newWords)

        println("Added words:")
        addedWords.foreach { word: Word =>
          println(" - " + word + ": " + scorer.scoreWord(word))
        }

        val points = scorer.computeTurnScore(addedWords) match {
          case Success(points) => points
          case Failure(NoSuchWordError(word)) =>
            println(s"Error: $word is not a word")
            0
          case Failure(err) =>
            println(s"Error: $err")
            0
        }

        println(s"Added score: ${Score}$points${AnsiColor.RESET}")
      }),

    TerminalCommand("t", Nil, "Shows a list all tiles and their point values",
      { args: List[String] =>
        // Show five letter-score pairs per row, to avoid taking
        // too many lines
        game.letterScores.toList.grouped(5).foreach { row: List[(Char, Int)] =>
          row.foreach {
            case (letter, score) =>
              print(s"${RackTile}$letter${AnsiColor.RESET} ${Score}$score${AnsiColor.RESET}")
              print(" | ")
          }
          println()
        }
      }),

    TerminalCommand("b", Nil, "Shows the current content of the board",
      { args: List[String] =>
        println("----- Old Board")
        player.printColoredBoard(turnBoard)

        val newBoard = turnBuilder.getBoard
        println("----- Current Board")
        player.printColoredBoard(newBoard)
      }),

    TerminalCommand("s", Nil, "Submit this turn",
      { args: List[String] =>
        isDone = true
      }),

    TerminalCommand("!", Nil, "Resets this turn",
      { args: List[String] =>
        turnBuilder.reload(turnBoard, turnTiles)
      }),

    TerminalCommand("h", Nil, "Show a help menu",
      { args: List[String] =>
        commands.foreach { cmd: TerminalCommand =>
          println(s"${cmd.name} ${cmd.args.mkString(" ")}\n    ${cmd.help}")
        }
      }))

  val commandTuples = commands.map { cmd: TerminalCommand => (cmd.name, cmd) }
  val commandMap = Map[String, TerminalCommand](commandTuples: _*)

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
        if (argSpec.length == args.length) {
          cmd.fn(args)
        } else {
          commandMap("h").fn(Nil)
        }
      case None =>
        commandMap("h").fn(Nil)
    }

    if (isDone) {
      Some(turnBuilder.getAdditions)
    } else {
      None
    }
  }
}
