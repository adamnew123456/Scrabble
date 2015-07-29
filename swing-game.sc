import java.util.concurrent.ArrayBlockingQueue
import java.awt.BorderLayout
import javax.swing._

import org.adamnew123456.scrabble._
import org.adamnew123456.scrabble.players._
import org.adamnew123456.scrabble.players.swing._

class RunClosure(closure: () => Unit) extends Runnable {
    def run = closure()
}

println("Starting game...")

val board = Board.empty(15, 15)
val config = new FileConfig("bin/letter-dist.txt", "bin/letter-score.txt", "bin/words.txt")
val returnQueue = new ArrayBlockingQueue[UIMessage](1)

val ui = new org.adamnew123456.scrabble.players.swing.UIManager(config, returnQueue)
val humanPlayer = new SwingPlayer("You", config, ui, returnQueue)
val humanObserver = new SwingObserver(ui)

val aiPlayer = new NaiveComputerPlayer("<Computer 1>", config)
val aiPlayer2 = new NaiveComputerPlayer("<Computer 2>", config)

val game = new Game(board, config, List(aiPlayer, humanPlayer, aiPlayer2))
game.attachObserver(humanObserver)

SwingUtilities.invokeLater(new RunClosure({ () =>
  println("Starting UI thread...")

  val main = new JFrame("Scrabble")
  main.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  main.getContentPane.setLayout(new BorderLayout())

  main.getContentPane.add(ui.window, BorderLayout.CENTER)

  main.pack()
  main.setVisible(true)
}))

val gameThread = new Thread(new RunClosure({ () =>
  println("Starting game...")
  game.run()
}))
gameThread.start
