package org.adamnew123456.scrabble.players.swing

import java.awt.event.MouseEvent
import javax.swing.{ BoxLayout, JPanel }
import scala.collection.mutable.HashSet

import org.adamnew123456.scrabble._

/**
 * The RackView is responsible for visualizing the contents of the player's
 * rack of tiles using several TileViews, and allowing the player to select
 * a tile by clicking on it.
 */
class RackView(config: Config, selection: TileSelection) extends JPanel {
  /**
   * Make sure that the RackView reflects the state of the selection, even
   * if somebody else changes it.
   */
  selection.attachObserver {
    case Some(tile) =>
      clearSelected
      setSelected(tile)
    case None =>
      clearSelected
  }
  
  /**
   * Makes each tile on the rack Active, thus removing any Selected highlights
   * from any of the tiles.
   */
  private def clearSelected = tiles.foreach {
    case (tileView: TileView) => 
      tileView.mode = Active
      tileView.repaint()
    case _ => ()
  }
  
  /**
   * Sets one tile of the given kind as selected. This ensures that, if one of
   * the given tiles is on the rack, the mode of the tile is changed. Returns true
   * if the selection was set, or false if no selected tile could be found.
   */
  private def setSelected(tile: Char): Boolean = {
    tiles.foreach {
      case (tileView: TileView) =>
        if (tileView.tile == tile) {
          tileView.mode = Selected
          tileView.repaint()
          return true
        }
      case _ => ()
    }
    false
  }  
  /**
   * Creates a new EmptyTileView, and sets up a ButtonListener to capture 
   * clicks to the TileView.
   */
  private def makeEmptyTileView = {
    val empty = new EmptyTileView()
    empty.addMouseListener(new ClosureButtonListener({ event: MouseEvent =>
      (event.getID, event.getButton) match {
        case (MouseEvent.MOUSE_CLICKED, MouseEvent.BUTTON1) =>
          selection.clear
        case _ => ()
      }
    }))
    empty
  }
  
  /**
   * Creates a TileView from a given character, and sets up a ButtonListener
   * to capture clicks to the TileView.
   */
  private def makeTileView(tile: Char) = {
    val score = config.letterScores(tile)
    val tileView = new TileView(tile, score, Active)
    tileView.addMouseListener(new ClosureButtonListener({ event: MouseEvent =>
      (event.getID, event.getButton) match {
        case (MouseEvent.MOUSE_CLICKED, MouseEvent.BUTTON1) =>
          selection.set(tileView.tile)
        case _ => ()
      }
    }))
    tileView
  }
  
  setLayout(new BoxLayout(this, BoxLayout.LINE_AXIS))
  var tiles: List[JPanel] = List.fill(7)(makeEmptyTileView)
  updateContainer
  
  /**
   * Gets all the new members of 'tiles' to be visible in this container.
   */
  private def updateContainer {
    tiles.foreach(add(_))
    revalidate
  }
  
  /**
   * This hooks into TurnBuilder, and updates the tiles displayed when the
   * rack changes.
   */
  val builderObserver = { builder: ObservableTurnBuilder =>
    println("Removing all old tiles")
    tiles.foreach(remove(_))
    
    val rackTiles = builder.getTiles.asList
    val emptyTiles = 7 - rackTiles.length
    
    println(s"Padding with $emptyTiles empty tiles")
    
    // The new group of tiles is a group of TileViews for each tile used in 
    // the rack, and a group of EmptyTileViews for each unused part of the rack
    tiles = 
      rackTiles.map(makeTileView(_)) ++ List.fill(emptyTiles)(makeEmptyTileView)
      
    updateContainer
    
    // If a tile is currently selected, then select one of the tiles on the rack
    // that matches it
    if (selection.hasSelection) {
      println("Updating selection")
      if (!setSelected(selection.get)) {
        // Somehow, we lost the selected tile. Since it isn't visible anywhere,
        // go ahead and unselect it
        println("Updating selection failed")
        selection.clear
      }
    }
  }
}