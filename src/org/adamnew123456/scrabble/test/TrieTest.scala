package org.adamnew123456.scrabble.test

import org.adamnew123456.scrabble.Trie

import org.junit.Test
import junit.framework.TestCase
import org.junit.Assert._

/**
 * Ensures that Tries work as intended.
 */
class TrieTest extends TestCase {
  var trie: Trie[Char] = _
  
  val words = List("cool", "cookie", "coke")
  
  override def setUp() {
    trie = new Trie[Char]()
    words.foreach { word => trie.add(word.toList) }
  }
  
  def testSuccessfulContains {
    words.foreach { word => assertTrue(trie.contains(word.toList)) }
    
    // "coo" is a prefix in the trie
    assertTrue(trie.contains("coo".toList, allowPrefix=true))
  }
  
  def testFailureContains {
    // "coo" is a prefix in the trie, but not actually a word
    assertFalse(trie.contains("coo".toList))
    
    // "wat" is not a prefix in the trie at all
    assertFalse(trie.contains("wat".toList))
    
    // "coolest" is too long to be in the tree, even if its prefix is in the
    // trie
    assertFalse(trie.contains("coolest".toList))
  }
}