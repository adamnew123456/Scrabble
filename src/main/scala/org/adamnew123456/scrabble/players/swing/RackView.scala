package org.adamnew123456.scrabble.players.swing

import java.awt.event.MouseEvent
import javax.swing.{ BoxLayout, JPanel }
import scala.collection.mutable.HashMap

import org.adamnew123456.scrabble._
/**
 * The RackView is responsible for visualizing the contents of the player's
 * rack of tiles using several TileViews.
 * 
 * Since the RackView can be in one of three modes, there are two selections
 * used to communicate with other components:
 * 
 * - The boardSelection is used to communicate with the BoardView, during the
 *   UIMode.Turn mode. It facilitates moving tiles from the rack to the board
 *   and vice versa.
 * - The replaceSelection is used to communicate with the main turn driver
 *   which tiles are selected for replacing during the UIMode.Replace mode.
 */
class RackView(config: Config, boardSelection: TileSelection, replaceSelection: MultiTileSelection) extends JPanel {
  /**
   * The RackView is capable of being in one of three modes:
   * 
   * - UIMode.Turn
   * - UIMode.Replace
   * - UIMode.Idle
   * 
   * These modes carry with them different behaviors; the current mode of the
   * RackView determines which should run. To avoid bulking up the various 
   * callbacks with dispatch code, this interface makes it possible to swap
   * out the current RackCallbackSuite and automatically have all callbacks
   * be changed.
   * 
   * Also, the modes themselves can carry state which isn't necessary for the 
   * other modes. This helps segregate state which shouldn't be mixing around
   * in the top-level of RackView
   */
  private trait RackCallbackSuite {
    /// This method is called when the suite is changed
    def reset: Unit
    
    def onBoardSelectionChanged(tile: Option[Char]): Unit
    def onReplaceSelectionChanged(tiles: TileGroup): Unit
    def onEmptyTileClick(event: MouseEvent): Unit
    def onTileClick(event: MouseEvent, tileView: TileView): Unit
    def onBuilderChange(builder: ObservableTurnBuilder): Unit
  }

  /**
   * The callback suite used when the player is placing tiles onto the board.
   * 
   * The main idea of this suite is that the player uses the RackView to
   * select one tile at a time.
   */
  private object TurnSuite extends RackCallbackSuite {
    def reset {
      boardSelection.clear
    }
    
    def onBoardSelectionChanged(tile: Option[Char]) {
      clearSelected
      if (tile.isDefined) {
        setSelected(tile.get)
      }
    }
    
    def onReplaceSelectionChanged(tiles: TileGroup) = ()
    
    def onEmptyTileClick(event: MouseEvent) {
      (event.getID, event.getButton) match {
        case (MouseEvent.MOUSE_CLICKED, MouseEvent.BUTTON1) =>
          boardSelection.clear
        case _ => ()
      }
    }
    
    def onTileClick(event: MouseEvent, tileView: TileView) {
      (event.getID, event.getButton) match {
        case (MouseEvent.MOUSE_CLICKED, MouseEvent.BUTTON1) =>
          boardSelection.set(tileView.tile)
        case _ => ()
      }
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
    
    def onBuilderChange(builder: ObservableTurnBuilder) {
      tiles.foreach(remove(_))
      
      val rackTiles = builder.getTiles.asList
      val emptyTiles = 7 - rackTiles.length
      
      // The new group of tiles is a group of TileViews for each tile used in 
      // the rack, and a group of EmptyTileViews for each unused part of the rack
      tiles = 
        rackTiles.map(makeTileView(_)) ++ List.fill(emptyTiles)(makeEmptyTileView)
        
      updateContainer
      
      // If a tile is currently selected, then select one of the tiles on the rack
      // that matches it
      if (boardSelection.hasSelection) {
        if (!setSelected(boardSelection.get)) {
          // Somehow, we lost the selected tile. Since it isn't visible anywhere,
          // go ahead and deselect it
          boardSelection.clear
        }
      }
    }
  }
  
  /**
   * The callback suite used when the player is selecting tiles to replace.
   * 
   * The main idea of this suite is that the player uses the RackView to select 
   * multiple tiles at one time, which can all be retrieved.
   * 
   * It can ignore changes to the selection, since it uses its own private list
   * of selected tiles; it also ignores clicks to empty tiles (since the 
   * selection buffer is useless here).
   */
  private object ReplaceTileSuite extends RackCallbackSuite {
    def reset {
      replaceSelection.clear
      clearSelected
    }
    
    def onBoardSelectionChanged(tile: Option[Char]) = ()
    
    def onReplaceSelectionChanged(tiles: TileGroup) {
      clearSelected
      setSelected(tiles)
    }
    
    def onEmptyTileClick(event: MouseEvent) = ()
    
    def onTileClick(event: MouseEvent, tileView: TileView) {
      (event.getID, event.getButton) match {
        case (MouseEvent.MOUSE_CLICKED, MouseEvent.BUTTON1) =>
          if (tileView.mode == Active) {
            tileView.mode = Selected
            replaceSelection += tileView.tile
          } else {
            tileView.mode = Active
            replaceSelection -= tileView.tile
          }
        case _ => ()
      }
    }
    
    def onBuilderChange(builder: ObservableTurnBuilder) {
      tiles.foreach(remove(_))
      
      val rackTiles = builder.getTiles.asList
      val emptyTiles = 7 - rackTiles.length
      
      // The new group of tiles is a group of TileViews for each tile used in 
      // the rack, and a group of EmptyTileViews for each unused part of the rack
      tiles = 
        rackTiles.map(makeTileView(_)) ++ List.fill(emptyTiles)(makeEmptyTileView)

      clearSelected
      replaceSelection.clear

      updateContainer
    }
    
    /**
     * This marks all tiles as Selected, which appear in the given TileGroup.
     */
    private def setSelected(toSelect: TileGroup) {
      var selectMap = toSelect.asMap
      
      selectMap.foreach {
        case (tile, count) =>
          val matchingTiles = tiles.filter {
            case tileView: TileView => tileView.tile == tile
            case _                  => false
          }
          
          val canSelect = matchingTiles.take(count)
          canSelect.foreach {
            case tileView: TileView => tileView.mode = Selected
          }
      }
    }
  }
  
  /**
   * The callback suite used when it is not the player's turn.
   * 
   * The main idea of this suite is to not react to anything, since the player
   * can't do anything while another player's turn is active.
   */
  private object IdleSuite extends RackCallbackSuite {
    def reset {
      clearSelected
    }
    
    def onBoardSelectionChanged(tile: Option[Char]) = ()
    def onReplaceSelectionChanged(tiles: TileGroup) = ()
    def onEmptyTileClick(event: MouseEvent) = ()
    def onTileClick(event: MouseEvent, tileView: TileView) = ()
    def onBuilderChange(builder: ObservableTurnBuilder) = ()
  }
  
  private var currentSuite: RackCallbackSuite = TurnSuite
  
  /**
   * Sets the mode of the RackView.
   * 
   * Using UIMode.Turn allows the user to select single tiles, while 
   * UIMode.Replace allows the user to select multiple tiles at once.
   * UIMode.Idle prevents the player from selecting anything.
   */
  def setMode(mode: UIMode.Type) = {
    mode match {
      case UIMode.Turn    => currentSuite = TurnSuite
      case UIMode.Replace => currentSuite = ReplaceTileSuite
      case UIMode.Idle    => currentSuite = IdleSuite
    }
    
    currentSuite.reset
  }
  
  /*
   * Make sure that the RackView reflects the state of the selection, even
   * if somebody else changes it.
   */
  boardSelection.attachObserver(currentSuite.onBoardSelectionChanged(_))
  replaceSelection.attachObserver(currentSuite.onReplaceSelectionChanged(_))
  
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
   * Creates a new EmptyTileView, and sets up a ButtonListener to capture 
   * clicks to the TileView.
   */
  private def makeEmptyTileView = {
    val empty = new EmptyTileView()
    empty.addMouseListener(new ClosureButtonListener({ event: MouseEvent =>
      currentSuite.onEmptyTileClick(event)
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
      currentSuite.onTileClick(event, tileView)
    }))
    tileView
  }
  
  // Start out with 7 empty tiles, until we get the correct state of the rack
  setLayout(new BoxLayout(this, BoxLayout.LINE_AXIS))
  var tiles: List[JPanel] = List.fill(7)(makeEmptyTileView)
  updateContainer
  
  /**
   * Gets all the members of 'tiles' to be visible in this container.
   */
  protected def updateContainer {
    tiles.foreach(add(_))
    revalidate
  }
  
  /**
   * This hooks into TurnBuilder, and updates the tiles displayed when the
   * rack changes.
   */
  val builderObserver = currentSuite.onBuilderChange(_)
}
