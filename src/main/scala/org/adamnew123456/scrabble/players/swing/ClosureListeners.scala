/*
 * These are a group of classes which extend various Listener classes in AWT,
 * but make it possible to use functions rather than subclassing to handle 
 * events.
 */

package org.adamnew123456.scrabble.players.swing

import java.awt.event._

/**
 * Makes an ActionListener from a function.
 */
class ClosureActionListener(closure: ActionEvent => Unit) extends ActionListener {
  override def actionPerformed(event: ActionEvent) = closure(event)
}

/**
 * Makes a MouseListener, which reacts to an event by running a function.
 * 
 * Note that the type of event is given as a part of the ButtonEvent structure.
 */
class ClosureButtonListener(closure: MouseEvent => Unit) extends MouseListener {
  override def mouseClicked(event: MouseEvent) = closure(event)
  override def mouseEntered(event: MouseEvent) = closure(event)
  override def mouseExited(event: MouseEvent) = closure(event)
  override def mousePressed(event: MouseEvent) = closure(event)
  override def mouseReleased(event: MouseEvent) = closure(event)
}