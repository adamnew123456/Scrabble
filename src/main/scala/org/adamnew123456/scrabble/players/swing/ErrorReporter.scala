package org.adamnew123456.scrabble.players.swing

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
 * After a delay, the SwingErrorReporter will set its error to None and notify all
 * observers. Note that this delay is reset every time a new error comes in.
 */
class SwingErrorReporter(delayMS: Int, reporter: ErrorReporter) extends JLabel("OK") {
  val notifyReset = new ClosureActionListener({ _ =>
    reporter.clear
  })
  
  val timer = new Timer(delayMS, notifyReset)
  timer.setRepeats(false)
  
  reporter.attachObserver {
    case Some(error) => 
      setText(error.toString)
      timer.stop
      timer.start
    case None        => 
      setText("OK")
      timer.stop
  }
}