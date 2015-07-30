package org.adamnew123456.scrabble.players.swing

import java.util.concurrent.ArrayBlockingQueue
import javax.swing._

import org.adamnew123456.scrabble._
import org.adamnew123456.scrabble.players._
import org.adamnew123456.scrabble.players._

/**
 * This gives the player the option of choosing how many AI players they want,
 * before letting him start the game.
 */
object GameSetup extends JFrame {
  getContentPane.setLayout(new BoxLayout(this.getContentPane, BoxLayout.Y_AXIS))
  
  val board = Board.empty(15, 15)
  val config = new ResourceConfig()
  val returnQueue = new ArrayBlockingQueue[UIMessage](1)
  
  val ui = new org.adamnew123456.scrabble.players.swing.UIManager(config, returnQueue)
  val playerObserver = new SwingObserver(ui)
  
  val playerName = new JTextField("Enter Your Name")
  val numAIPlayers = new JSpinner(new SpinnerNumberModel(2, 0, 10, 1))
  val start = new JButton("Start Game")
  start.addActionListener(new ClosureActionListener({ _ =>
    // Set up the necessary data to run the game
    val name = playerName.getText
    val numAI = numAIPlayers.getValue.asInstanceOf[Int]
    val aiPlayers: List[BasePlayer] = 1.to(numAI).map { num: Int =>
      new NaiveComputerPlayer(s"<Computer $num>", config)
    }.toList
    
    val player: BasePlayer = new SwingPlayer(name, config, ui, returnQueue)
    
    // Swap out the game setup UI for the game UI itself
    getContentPane.removeAll
    getContentPane.add(ui.window)
    getContentPane.revalidate
    
    val game = new Game(board, config, player :: aiPlayers)
    game.attachObserver(playerObserver)
    val gameThread = new Thread(new ClosureRunnable(game.run()))
    gameThread.start()
  }))
  
  def main(args: Array[String]) {
    getContentPane.add(playerName)
    getContentPane.add(numAIPlayers)
    getContentPane.add(start)
    
    setVisible(true)
    pack()
  }
}