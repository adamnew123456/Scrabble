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

class SelectionView(selection: TileSelection) extends JLabel("No tile selected") {
  selection.attachObserver { tile: Option[Char] =>
    println(s"Selection changed: $tile")
    tile match {
      case Some(tile) => setText("Selected: " + tile)
      case None       => setText("No tile selected")
    }
  }
}

class SelectionEdit(selection: TileSelection) extends JPanel {
  setLayout(new BoxLayout(this, BoxLayout.Y_AXIS))

  val input = new JTextField("")
  val change = new JButton("Change Selection")
  change.addActionListener(new ActionClosure({ _ =>
    val newSelection = input.getText
    if (newSelection != "") {
      println(s"Setting selection: ${newSelection(0)}")
      selection.set(newSelection(0))
    } else {
      println("Clearing selection")
      selection.clear
    }
  }))

  add(input)
  add(change)
}

class RackEdit(turnBuilder: MockObservableTurnBuilder) extends JPanel {
  setLayout(new BoxLayout(this, BoxLayout.X_AXIS))

  val tiles = new JTextField("abcdefg")
  val set = new JButton("Set Tiles")
  set.addActionListener(new ActionClosure({ _ =>
    println("Setting tiles: ${tiles.getText}")
    val tileString = tiles.getText.take(7)

    val rack = TileGroup.fromTraversable(tileString)
    turnBuilder.setRack(rack)
  }))

  add(tiles)
  add(set)
}

SwingUtilities.invokeLater(new RunClosure({ () =>
  val main = new JFrame("RackView Test")
  main.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  main.getContentPane.setLayout(new BorderLayout())

  val selection = new TileSelection()
  val turnBuilder = new MockObservableTurnBuilder()

  val selectionView = new SelectionView(selection)
  val selectionEdit = new SelectionEdit(selection)
  val rackEdit = new RackEdit(turnBuilder)

  val config = new FileConfig("bin/letter-dist.txt", "bin/letter-score.txt", "bin/words.txt")
  val rackView = new RackView(config, selection)

  turnBuilder.attachObserver(rackView.builderObserver)

  main.getContentPane.add(rackEdit, BorderLayout.SOUTH)
  main.getContentPane.add(selectionView, BorderLayout.NORTH)
  main.getContentPane.add(selectionEdit, BorderLayout.EAST)
  main.getContentPane.add(rackView, BorderLayout.CENTER)

  main.pack()
  main.setVisible(true)
}))
