package org.adamnew123456.scrabble.players

import scala.io.AnsiColor

/**
 * This stores color information, to make it easy to print colors on the
 * terminal consistently.
 */
object TerminalColorScheme {
  val Score = AnsiColor.BOLD + AnsiColor.RED
  val BoardTile = AnsiColor.BOLD + AnsiColor.CYAN
  val BoardEmpty = AnsiColor.WHITE
  val RackTile = AnsiColor.BOLD + AnsiColor.WHITE
}