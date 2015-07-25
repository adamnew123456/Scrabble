import java.awt.BorderLayout
import java.awt.event._
import javax.swing._
import scala.collection.mutable.HashMap
import scala.util.{ Try, Success, Failure }

import org.adamnew123456.scrabble._
import org.adamnew123456.scrabble.players.swing._

class RunClosure(closure: () => Unit) extends Runnable {
  def run = closure()
}

class ActionClosure(closure: ActionEvent => Unit) extends ActionListener {
  def actionPerformed(event: ActionEvent) = closure(event)
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

/**
 * This is like the default TurnBuilder, but it doesn't change the
 * tile rack. This is because the RackView isn't included in this test, and
 * I want to avoid complicating it by adding it. 
 *
 * It also omits a lot of checks that the original TurnBuilder does, so it may
 * well break. However, this isn't what's under test here, so that doesn't
 * matter.
 */
class TestTurnBuilder extends BaseObservable[ObservableTurnBuilder] with MutableTurnBuilder with ObservableTurnBuilder {
  val stockBoardTiles = Map((7, 7) -> 'c', (7, 8) -> 'a', (7, 9) -> 't')
  val stockBoard = Board.empty(15, 15).addCharacters(stockBoardTiles) match {
    case Success(board) => board
    case Failure(exn) =>
      println(s"ERROR ON LINE 62: $exn")
      ???
  }
  val stockTiles = TileGroup.fromTraversable("abcdefg")

  val additions = HashMap[(Int, Int), Char]()
  def getBoard = stockBoard.addCharacters(additions.toMap) match {
    case Success(board) => board
    case Failure(exn) =>
      println(s"ERROR ON LINE 70: $exn")
      ???
  }

  def getTiles = stockTiles
  def getAdditions = additions.toMap

  def addTiles(tiles: Map[(Int, Int), Char]): Try[Unit] = {
    val result = for {
      tempBoard <- stockBoard.addCharacters(additions.toMap)
      result <- tempBoard.addCharacters(tiles)
    } yield result

    result match {
      case Success(_) =>
        additions ++= tiles
        notifyObservers(this)
        Success(())
      case Failure(exn) =>
        Failure(exn)
    }
  }

  def addWord(word: String, location: (Int, Int), direction: Direction.Type): Try[Unit] = 
    ???

  def removeTiles(spaces: Set[(Int, Int)]) = {
    additions --= spaces
    notifyObservers(this)
    Success(())
  }

  def reload(board: Board, rack: TileGroup) =
    additions.clear
}

SwingUtilities.invokeLater(new RunClosure({ () =>
  val main = new JFrame("BoardView Test")
  main.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  main.getContentPane.setLayout(new BorderLayout())

  val selection = new TileSelection()
  val turnBuilder = new TestTurnBuilder()
  val errorReporter = new ErrorReporter()

  val selectionView = new SelectionView(selection)
  val selectionEdit = new SelectionEdit(selection)
  val errorView = new SwingErrorReporter(5000, errorReporter)

  val config = new FileConfig("bin/letter-dist.txt", "bin/letter-score.txt", "bin/words.txt")
  val boardView = new BoardView(config, selection, errorReporter, turnBuilder)

  turnBuilder.attachObserver(boardView.builderObserver)
  
  val modeSwitcher = new ModeSwitcher(boardView.setMode(_))

  // Get the initial part of the board set up
  boardView.builderObserver(turnBuilder)

  main.getContentPane.add(errorView, BorderLayout.SOUTH)
  main.getContentPane.add(selectionView, BorderLayout.NORTH)
  main.getContentPane.add(selectionEdit, BorderLayout.EAST)
  main.getContentPane.add(modeSwitcher, BorderLayout.WEST)
  main.getContentPane.add(boardView, BorderLayout.CENTER)

  main.pack()
  main.setVisible(true)
}))
