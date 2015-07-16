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

class MockObservableTurnBuilder extends BaseObservable[ObservableTurnBuilder] with ObservableTurnBuilder {
  private var rack: TileGroup = _
  private val emptyBoard = Board.empty(15, 15)

  def setRack(newRack: TileGroup) = {
    rack = newRack
    println(s"Rack changed to ${newRack.asMap}")
    notifyObservers(this)
  }

  def getBoard = emptyBoard
  def getTiles = rack
  def getAdditions = Map()
}

SwingUtilities.invokeLater(new RunClosure({ () =>
  val main = new JFrame("RackView Test")
  main.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  main.getContentPane.setLayout(new BorderLayout())

  /*
   * This displays the currently selected tile.
   */
  val selection = new TileSelection()
  val selectedTile = new JLabel("No tile selected")
  
  selection.attachObserver { tile: Option[Char] =>
    println(s"Selection changed: $tile")
    tile match {
      case Some(tile) => selectedTile.setText(tile.toString)
      case None       => selectedTile.setText("No tile selected")
    }
  }

  /*
   * This runs the observable inside of the RackView, and changes the currently
   * available tiles.
   */
  val tileChangeGroup = new JPanel()
  tileChangeGroup.setLayout(new BoxLayout(tileChangeGroup, BoxLayout.X_AXIS))

  val currentTiles = new JTextField("abcdefg")
  tileChangeGroup.add(currentTiles)

  val mockTurnBuilder = new MockObservableTurnBuilder()
  val setTiles = new JButton("Set Tiles")
  setTiles.addActionListener(new ActionClosure({ _ =>
    println(s"Setting tiles: ${currentTiles.getText}")
    val tileString = currentTiles.getText.take(7)
    val rack = TileGroup.fromTraversable(tileString)
    mockTurnBuilder.setRack(rack)
  }))
  tileChangeGroup.add(setTiles)

  val config = new FileConfig("bin/letter-dist.txt", "bin/letter-score.txt", "bin/words.txt")
  val rackView = new RackView(config, selection)

  mockTurnBuilder.attachObserver(rackView.builderObserver)

  main.getContentPane.add(tileChangeGroup, BorderLayout.SOUTH)
  main.getContentPane.add(selectedTile, BorderLayout.NORTH)
  main.getContentPane.add(rackView, BorderLayout.CENTER)

  main.pack()
  main.setVisible(true)
}))
