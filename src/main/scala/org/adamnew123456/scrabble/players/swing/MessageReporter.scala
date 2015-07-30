package org.adamnew123456.scrabble.players.swing

import java.awt.event.MouseEvent
import javax.swing.{ JLabel, Timer }

import org.adamnew123456.scrabble.BaseObservable

/**
 * This is responsible for taking errors from the BoardView. The default
 * implementation, SwingErrorReporter, is harder to test, which is whythis
 * is factored into a separate interface. 
 */
class MessageReporter extends BaseObservable[Option[String]] {
  var message: Option[String] = None
  
  def clear {
    if (message.isDefined) {
      message = None
      notifyObservers(message)
    }
  }
    
  def report(msg: String) {
    message = Some(msg)
    notifyObservers(message)
  }
}

/**
 * This is responsible for transporting errors from one part of the program
 * to another - primarily, from the BoardView to the status bar.
 * 
 * When clicked, the current message is reset - until then, the current error 
 * is displayed.
 */
class SwingMessageReporter(reporter: MessageReporter) extends JLabel() {
  addMouseListener(new ClosureButtonListener({ event: MouseEvent =>
    (event.getID, event.getButton) match {
      case (MouseEvent.MOUSE_CLICKED, MouseEvent.BUTTON1) =>
        reporter.clear
      case _ => ()
    }
  }))
  
  reporter.attachObserver {
    case Some(message) => 
      setText(message)
    case None        => 
      setText("")
  }
}