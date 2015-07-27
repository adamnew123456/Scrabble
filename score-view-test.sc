import java.awt.BorderLayout
import java.awt.event._
import javax.swing._

import java.util.concurrent.Semaphore

import org.adamnew123456.scrabble.players.swing.ScoreView

class RunClosure(closure: () => Unit) extends Runnable {
  def run = closure()
}

class ActionClosure(closure: ActionEvent => Unit) extends ActionListener {
  def actionPerformed(event: ActionEvent) = closure(event)
}

class CommandProcessor(fn: String => String) extends JTextField {
  addActionListener(new ActionClosure({ _ =>
    val result = fn(getText)
    setText(result)
  }))
}

SwingUtilities.invokeLater(new RunClosure({ () =>
  val window = new JFrame("ScoreView Test")
  window.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  window.getContentPane.setLayout(new BorderLayout())

  var scores = Map[String, Int]()
  val scoreView = new ScoreView()
  val cmdProc = new CommandProcessor({
    case "exit" => 
      System.exit(0)
      "OK"
    case input =>
      input.split(" ") match {
        case Array(player, score) =>
          scores += ((player, score.toInt))
          scoreView.update(scores)
          "OK"
        case _ =>
          "Expected 'exit' or '<player> <score>'"
      }
  })

  window.add(scoreView, BorderLayout.CENTER)
  window.add(cmdProc, BorderLayout.SOUTH)

  window.pack()
  window.setVisible(true)
}))
