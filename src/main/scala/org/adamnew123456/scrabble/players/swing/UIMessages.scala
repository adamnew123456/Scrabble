package org.adamnew123456.scrabble.players.swing

import org.adamnew123456.scrabble.TileGroup

/**
 * These are the messages that the UI thread sends to the game thread.
 * 
 * See the SwingPlayer documentation for how these fit in, and why they are
 * necessary.
 */
trait UIMessage
case class TurnMessage(turn: Map[(Int, Int), Char]) extends UIMessage
case class TileReplaceMessage(tiles: TileGroup) extends UIMessage