import java.awt._
import java.awt.event._
import javax.swing._

import org.adamnew123456.scrabble._
import org.adamnew123456.scrabble.players.swing._

class RunClosure(closure: () => Unit) extends Runnable {
  def run = closure()
}

class ActionClosure(closure: ActionEvent => Unit) extends ActionListener {
  def actionPerformed(event: ActionEvent) = closure(event)
}

class ErrorEdit(reporter: ErrorReporter) extends JPanel {
  setLayout(new BoxLayout(this, BoxLayout.Y_AXIS))

  val input = new JTextField("")
  val change = new JButton("Report Error")
  change.addActionListener(new ActionClosure({ _ =>
    val errorText = input.getText
    if (errorText != "") {
      println(s"Setting error: ${errorText}")
      reporter.report(new Throwable(errorText))
    }
  }))

  add(input)
  add(change)
}

SwingUtilities.invokeLater(new RunClosure({ () =>
  val main = new JFrame("RackView Test")
  main.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  main.getContentPane.setLayout(new BorderLayout())

  val reporter = new ErrorReporter()
  val errorView = new SwingErrorReporter(reporter)
  val errorEdit = new ErrorEdit(reporter)

  reporter.attachObserver {
    case Some(err) => println(s"Error: $err")
    case None      => println("No error")
  }

  main.getContentPane.add(errorView, BorderLayout.SOUTH)
  main.getContentPane.add(errorEdit, BorderLayout.CENTER)

  main.pack()
  main.setVisible(true)
}))
