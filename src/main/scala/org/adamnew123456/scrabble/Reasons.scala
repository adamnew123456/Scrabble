package org.adamnew123456.scrabble

/**
 * These signify all the possible reasons why a game could end.
 */
trait EndReason extends Exception

/**
 * The game cannot continue if the tile bag is empty.
 */
case object EmptyBagEnding extends EndReason {
  override def toString = "No tiles left in bag"
}

/**
 * A player has deliberately quit the game.
 */
case class ForfeitEnding(player: String) extends EndReason {
  override def toString = "Player $player has forfeited"
}

/**
 * These signify the reasons why a player's turn might be rejected.
 */
trait TurnRejectReason

/**
 * A turn might be rejected because the words that the turn adds aren't
 * connected to any other words.
 */
case object DisconnectReject extends TurnRejectReason {
  override def toString = "Not all tiles on the board connect"
}

/**
 * A turn might also be rejected for a reason which is covered by an existing
 * exception.
 */
case class ExceptionReject(exn: Throwable) extends TurnRejectReason {
  override def toString = exn.toString
}