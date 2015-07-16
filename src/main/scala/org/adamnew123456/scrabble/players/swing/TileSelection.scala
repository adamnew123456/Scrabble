package org.adamnew123456.scrabble.players.swing

import scala.collection.mutable.HashSet

import org.adamnew123456.scrabble.BaseObservable

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
  def set(tile: Char) = {
    selection = Some(tile)
    notifyObservers(Some(tile))
  }  
  /**
   * Clears the current selection.
   */
  def clear = {
    selection = None
    notifyObservers(None)
  }
}