package org.adamnew123456.scrabble.players.swing

import scala.collection.mutable.HashMap

import org.adamnew123456.scrabble.{ BaseObservable, TileGroup }

/**
 * The TileSelection is used to communicate, between two components, that a 
 * particular tile is currently selected.
 */
class TileSelection extends BaseObservable[Option[Char]] {
  var selection: Option[Char] = None
  
  /**
   * Gets the selection, but as an Option.
   */
  def maybeGet = selection
  
  /**
   * Gets the current selected tile.
   */
  def get = selection.get
  
  /**
   * Tests to see if there is a currently selected tile.
   */
  def hasSelection = selection.isDefined
  
  /**
   * Sets the current selection.
   */
  def set(tile: Char) =
    selection match {
      case Some(`tile`) => ()
      case _ =>
        selection = Some(tile)
        notifyObservers(selection)
    }  
  
  /**
   * Clears the current selection.
   */
  def clear = {
    if (selection.isDefined) {
      selection = None
      notifyObservers(selection)
    }
  }
}

/**
 * The MultiTileSelection is used to communicate that a group of tiles is
 * selected between two components.
 */
class MultiTileSelection extends BaseObservable[TileGroup] {
  val selection = HashMap[Char, Int]()
  
  /**
   * Gets the current selection as a TileGroup.
   */
  def get = TileGroup.fromMap(selection.toMap)
  
  /**
   * Adds a tile to the current selection.
   */
  def +=(tile: Char) = {
    if (selection.contains(tile)) {
      selection(tile) += 1
    } else {
      selection(tile) = 1
    }
    
    notifyObservers(get)
  }
  
  /**
   * Removes a tile from the current selection. Returns true if the tile was
   * removed, or false if the tile was not in the selection to begin with.
   */
  def -=(tile: Char) =
    if (selection.contains(tile) && selection(tile) > 0) {
      selection(tile) -= 1
      notifyObservers(get)
      true
    } else {
      false
    }
  
  /**
   * Clears the current selection.
   */
  def clear = {
    if (!selection.isEmpty) {
      selection.clear
      notifyObservers(get)
    }
  }
}