import java.awt.BorderLayout
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
  def getBaseBoard = emptyBoard
  def getTiles = rack
  def getAdditions = Map()
}

class SelectionView(selection: TileSelection, multiSelection: MultiTileSelection) extends JPanel {
  setLayout(new BoxLayout(this, BoxLayout.Y_AXIS))

  val selectionLabel = new JLabel("Single:")
  selection.attachObserver { tile: Option[Char] =>
    println(s"Single selection changed: $tile")
    tile match {
      case Some(tile) => selectionLabel.setText("Single: " + tile)
      case None       => selectionLabel.setText("Single:")
    }
  }

  val multiLabel = new JLabel("Multiple:")
  multiSelection.attachObserver { tiles: TileGroup =>
    val tileStr = tiles.asList.mkString(",")
    println("Multiple selection changed: " + tileStr)
    multiLabel.setText("Multiple: " + tileStr)
  }

  add(selectionLabel)
  add(multiLabel)
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

class ModeSwitcher(callback: UIMode.Type => Unit) extends JPanel {
  setLayout(new BoxLayout(this, BoxLayout.Y_AXIS))

  val group = new ButtonGroup()
  val List(makeReplace, makeTurn, makeIdle) = 
    List(UIMode.Replace, UIMode.Turn, UIMode.Idle).zip(List("Replace", "Turn", "Idle")).map {
      case (mode, label) =>
        val button = new JRadioButton(label, false)
        button.addActionListener(new ActionClosure({ _ =>
          callback(mode)
        }))

        group.add(button)
        add(button)
        button
    }

  makeTurn.setSelected(true)
}

SwingUtilities.invokeLater(new RunClosure({ () =>
  val main = new JFrame("RackView Test")
  main.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  main.getContentPane.setLayout(new BorderLayout())

  val selection = new TileSelection()
  val multiSelection = new MultiTileSelection()
  val turnBuilder = new MockObservableTurnBuilder()

  val selectionView = new SelectionView(selection, multiSelection)
  val selectionEdit = new SelectionEdit(selection)
  val rackEdit = new RackEdit(turnBuilder)

  val config = new FileConfig("bin/letter-dist.txt", "bin/letter-score.txt", "bin/words.txt")
  val rackView = new RackView(config, selection, multiSelection)

  turnBuilder.attachObserver(rackView.builderObserver)

  val modeSwitcher = new ModeSwitcher(rackView.setMode(_))

  main.getContentPane.add(rackEdit, BorderLayout.SOUTH)
  main.getContentPane.add(selectionView, BorderLayout.NORTH)
  main.getContentPane.add(selectionEdit, BorderLayout.EAST)
  main.getContentPane.add(modeSwitcher, BorderLayout.WEST)
  main.getContentPane.add(rackView, BorderLayout.CENTER)

  main.pack()
  main.setVisible(true)
}))
