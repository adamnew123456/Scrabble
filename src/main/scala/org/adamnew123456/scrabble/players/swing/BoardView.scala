package org.adamnew123456.scrabble.players.swing

import java.awt.{ Dimension, GridLayout }
import java.awt.event.MouseEvent
import javax.swing.JPanel
import scala.collection.mutable.{ HashMap, HashSet } 
import scala.util.{ Try, Success, Failure }

import org.adamnew123456.scrabble._

/**
 * The BoardView is responsible for visualizing the contents of the game board,
 * and allowing the player to move tiles on and off the game board by clicking
 * on it.
 */
class BoardView(config: Config, selection: TileSelection, reporter: MessageReporter, builder: MutableTurnBuilder) extends JPanel {
  // Keep a hold on the GridLayout, since we need to resize it later when we
  // get an actual Board
  val gridLayout = new GridLayout(1, 1)
  setLayout(gridLayout)
  
  private val tiles = HashMap[(Int, Int), JPanel]()
  
  // Start out with a blank widget, and fill it in as we get notifications
  tiles((0, 0)) = new EmptyTileView(true)
  updateContainer(1, 1)
  
  private var currentlyEnabled = true
  def setMode(mode: UIMode.Type) = mode match {
    case UIMode.Turn => currentlyEnabled = true
    case _           => currentlyEnabled = false
  }

  var currentSize = (40, 40)
  override def getMinimumSize = getPreferredSize
  override def getMaximumSize = getPreferredSize
  override def getPreferredSize = new Dimension(currentSize._1, currentSize._2)
  
  /**
   * Adds all the tiles to this widget, in the proper location on the grid, and
   * makes them visible.
   */
  private def updateContainer(width: Int, height: Int) {
    for (col <- 0.to(width - 1) ;
         row <- 0.to(height - 1)) {
      val component = tiles((col, row))
      add(component)
    }
    
    currentSize = (width * 40, height * 40)
    revalidate
  }
  
  /**
   * Creates a new active tile of the given letter, and at the given location.
   */
  private def makeActiveTile(col: Int, row: Int, tile: Char): TileView = {
    val score = config.letterScores(tile)
    val tileView = new TileView(tile, score, Active)
    val handler = new ClosureButtonListener({ event: MouseEvent =>
      if (currentlyEnabled) {
        (event.getID, event.getButton) match {
          case (MouseEvent.MOUSE_CLICKED, MouseEvent.BUTTON1) =>
            selection.clear
            
          case (MouseEvent.MOUSE_CLICKED, MouseEvent.BUTTON3) =>
            builder.removeTiles(Set((col, row))) match {
              case Failure(err) => reporter.report(err.toString)
              case _            => ()
            }
          case _ => ()
        }
      }
    })
    
    tileView.addMouseListener(handler)
    tileView
  }
  
  /**
   * Creates a new permanent tile of the given letter, and at the given location.
   */
  private def makePermanentTile(col: Int, row: Int, tile: Char): TileView = {
    val score = config.letterScores(tile)
    val tileView = new TileView(tile, score, Inactive)
    val handler = new ClosureButtonListener({ event: MouseEvent =>
      if (currentlyEnabled) {
        (event.getID, event.getButton) match {
          case (MouseEvent.MOUSE_CLICKED, MouseEvent.BUTTON1) =>
            selection.clear
            
          case _ => ()
        }
      }
    })
    
    tileView.addMouseListener(handler)
    tileView
  }
  
  /**
   * Creates a new blank tile at the given location.
   */
  private def makeEmptyTile(col: Int, row: Int, center: Boolean): EmptyTileView = {
    val emptyTile = new EmptyTileView(center)
    val handler = new ClosureButtonListener({ event: MouseEvent =>
      if (currentlyEnabled) {
        (event.getID, event.getButton) match {
          case (MouseEvent.MOUSE_CLICKED, MouseEvent.BUTTON1) =>
            if (selection.hasSelection) {
              val selected = selection.get
              selection.clear
              
              builder.addTiles(Map((col, row) -> selected)) match {
                case Failure(err) => reporter.report(err.toString)
                case _            => ()
              }
            }
          case _ => ()
        }
      }
    })
    
    emptyTile.addMouseListener(handler)
    emptyTile
  }
  
  /**
   * This hooks into TurnBuilder, and updates the contents of the board when
   * it changes.
   */
  val builderObserver = {
    observableBuilder: ObservableTurnBuilder =>
      // Remove old widgets, both from the tiles map, but also the widget 
      // hierarchy
      tiles.values.map(remove(_))
      tiles.clear
      
      val board = observableBuilder.getBaseBoard
      val additions = observableBuilder.getAdditions
      
      // This has to be updated, since the default is 1x1 and no board would 
      // be that small
      gridLayout.setColumns(board.width)
      gridLayout.setRows(board.height)
      
      // There are three kinds of spaces on the board:
      // - Unoccupied spaces
      // - Spaces which are occupied because of previous moves. Players cannot
      //   change the tiles on these spaces.
      // - Spaces which are occupied because of the current move. Players can
      //   change the tiles on these spaces.
      val blanks = HashSet[(Int, Int)]()
      val permanent = HashMap[(Int, Int), Char]()
      val turn = HashMap[(Int, Int), Char]()
      
      for (col <- 0.to(board.width - 1) ; row <- 0.to(board.height - 1)) {
        if (additions.contains((col, row))) {
          turn((col, row)) = additions((col, row))
        } else if (board(col, row).isDefined) {
          permanent((col, row)) = board(col, row).get
        } else {
          blanks += ((col, row))
        }
      }
      
      /*
       * Blanks can be left-clicked, at which point the selected tile will be
       * placed on the board at that location (if there is a selected tile).
       */
      blanks.map {
        case (col, row) =>
          val center = (row == board.width / 2 && col == board.height / 2)
          val emptyTileView = makeEmptyTile(col, row, center)
          tiles((col, row)) = emptyTileView
      }
      
      /*
       * Permanent tiles can be left-clicked, which clears the selection.
       */
      permanent.map {
        case ((col, row), tile) =>
          val tileView = makePermanentTile(col, row, tile)
          tiles((col, row)) = tileView
      }
      
      /*
       * Tiles added during the turn can be left-clicked, which clears the
       * selection, or right-clicked, which removes them from the board.
       */
      turn.map {
        case ((col, row), tile) =>
          val tileView = makeActiveTile(col, row, tile)
          tiles((col, row)) = tileView
      }
      
      updateContainer(board.width, board.height)
  }
}
