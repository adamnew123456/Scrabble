package org.adamnew123456.scrabble.players.swing

/**
 * The UI can be in different modes, based upon what part of the turn the player
 * is on.
 * 
 * - The Replace state is used to indicate that the player is choosing what
 *   tiles on the rack to replace.
 * - The Turn state is used to indicate that the player can place tiles on the
 *   board.
 * - The Idle state is used to indicate that another player is making a move,
 *   and the UI cannot be used. This state arises because the UI is run in a
 *   separate thread.
 */
object UIMode extends Enumeration {
  type Type = Value
  val Replace, Turn, Idle = Value
}