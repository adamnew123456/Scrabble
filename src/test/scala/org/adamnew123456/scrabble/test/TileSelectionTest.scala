package org.adamnew123456.scrabble.test

import org.adamnew123456.scrabble.players.swing.TileSelection

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
    assertEquals(observer.notifyValue, Some('a'))
  }
  
  def testClearSelection {
    selection.clear
    assertFalse(selection.hasSelection)
    assertEquals(selection.maybeGet, None)
    
    assertTrue(observer.wasNotified)
    assertEquals(observer.notifyValue, None)
  }
}