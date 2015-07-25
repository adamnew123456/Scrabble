package org.adamnew123456.scrabble.test

import org.adamnew123456.scrabble.TileGroup
import org.adamnew123456.scrabble.players.swing.{ MultiTileSelection, TileSelection }

import org.junit.Test
import junit.framework.TestCase
import org.junit.Assert._

/**
 * This ensures that the TileSelection works as intended.
 */
class TileSelectionTest extends TestCase {
  var selection: TileSelection = _
  
  class SelectionObserver {
    var wasNotified = false
    var notifyValue: Option[Char] = _
    
    val observe = { tile: Option[Char] =>
      notifyValue = tile
      wasNotified = true
    }
  }
  
  var observer: SelectionObserver = _
  
  override def setUp {
    selection = new TileSelection()
    observer = new SelectionObserver()
    
    selection.attachObserver(observer.observe)
  }
  
  def testSetSelection {
    selection.set('a')
    assertTrue(selection.hasSelection)
    assertEquals(selection.maybeGet, Some('a'))
    assertEquals(selection.get, 'a')
    
    assertTrue(observer.wasNotified)
    assertEquals(observer.notifyValue, selection.maybeGet)
    
    observer.wasNotified = false
    // Ensure that the observers are only notified if the selection changes
    selection.set('a')
    assertFalse(observer.wasNotified)
  }
  
  def testClearSelection {
    // Prime the selection, so that the first clear isn't skipped
    selection.set('a')
    observer.wasNotified = false
    
    selection.clear
    assertFalse(selection.hasSelection)
    assertEquals(selection.maybeGet, None)
    
    assertTrue(observer.wasNotified)
    assertEquals(observer.notifyValue, selection.maybeGet)
    
    observer.wasNotified = false
    // Clearing the rack a second time should not set off the observers
    selection.clear
    assertFalse(observer.wasNotified)
  }
}

/**
 * This ensures that the MultiTileSelection works as intended.
 */
class MultiTileSelectionTest extends TestCase {
  var selection: MultiTileSelection = _
  
  class SelectionObserver {
    var wasNotified = false
    var notifyValue: TileGroup = _
    
    val observe = { tiles: TileGroup =>
      notifyValue = tiles
      wasNotified = true
    }
  }
  
  var observer: SelectionObserver = _
  
  override def setUp {
    selection = new MultiTileSelection()
    observer = new SelectionObserver()
    
    selection.attachObserver(observer.observe)
  }
  
  def testAddTiles() {
    // Test adding a new value that wasn't in the selection before
    selection += 'a'
    assertTrue(observer.wasNotified)
    assertEquals(observer.notifyValue, TileGroup.fromTraversable("a"))
    assertEquals(selection.get, observer.notifyValue)
    
    observer.wasNotified = false
    // Test adding a new value that was already in the selection
    selection += 'a'
    assertTrue(observer.wasNotified)
    assertEquals(observer.notifyValue, TileGroup.fromTraversable("aa"))
    assertEquals(selection.get, observer.notifyValue)
  }
  
  def testRemoveTiles() {
    selection += 'a'
    selection += 'a'
    observer.wasNotified = false
    
    // Test removing one of >1 tiles of the same kind
    selection -= 'a'
    assertTrue(observer.wasNotified)
    assertEquals(observer.notifyValue, TileGroup.fromTraversable("a"))
    assertEquals(selection.get, observer.notifyValue)
    
    observer.wasNotified = false
    // Test removing the only tile of a kind
    selection -= 'a'
    assertTrue(observer.wasNotified)
    assertEquals(observer.notifyValue, TileGroup.fromTraversable(""))
    assertEquals(selection.get, observer.notifyValue)
    
    observer.wasNotified = false
    // Finally, ensure that removing a non-existent tile does nothing
    selection -= 'a'
    assertFalse(observer.wasNotified)
    assertEquals(selection.get, TileGroup.fromTraversable(""))
  }
  
  def testClearTiles() {
    selection += 'a'
    selection += 'a'
    observer.wasNotified = false
    
    // Clearing the rack should remove the tiles on it
    selection.clear
    assertTrue(observer.wasNotified)
    assertEquals(observer.notifyValue, TileGroup.fromTraversable(""))
    assertEquals(selection.get, observer.notifyValue)
    
    // Clearing an empty rack should not trigger the observers
  }
}