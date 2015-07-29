package org.adamnew123456.scrabble.players.swing

import java.awt.event.MouseEvent
import javax.swing.{ JLabel, Timer }

import org.adamnew123456.scrabble.BaseObservable

/**
 * This is responsible for taking errors from the BoardView. The default
 * implementation, SwingErrorReporter, is harder to test, which is whythis
 * is factored into a separate interface. 
 */
class ErrorReporter extends BaseObservable[Option[Throwable]] {
  var error: Option[Throwable] = None
  
  def clear {
    if (error.isDefined) {
      error = None
      notifyObservers(error)
    }
  }
    
  def report(err: Throwable) {
    error = Some(err)
    notifyObservers(error)
  }
}

/**
 * This is responsible for transporting errors from one part of the program
 * to another - primarily, from the BoardView to the status bar.
 * 
 * When clicked, the current message is reset - until then, the current error 
 * is displayed.
 */
class SwingErrorReporter(reporter: ErrorReporter) extends JLabel("OK") {
  addMouseListener(new ClosureButtonListener({ event: MouseEvent =>
    (event.getID, event.getButton) match {
      case (MouseEvent.MOUSE_CLICKED, MouseEvent.BUTTON1) =>
        reporter.clear
      case _ => ()
    }
  }))
  
  reporter.attachObserver {
    case Some(error) => 
      setText(error.toString)
    case None        => 
      setText("OK")
  }
}