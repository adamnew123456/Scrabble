package org.adamnew123456.scrabble

import scala.annotation.tailrec
import scala.collection.mutable

/**
 * This implements a mutable Trie, which is used for the game's word list.
 * 
 * A Trie is a kind of tree which is used to represent a group of strings,
 * which makes for easy search and membership testing.
 * 
 *   h -* e -- l -* l -* o
 *        \ -- c -* k -- l -*e
 *        
 * Each node has information on whether it is a terminal node, and the datum
 * stored by that node.
 */
class Trie[T] {
  class TrieNode(val children: mutable.HashMap[T, TrieNode], var isTerminus: Boolean) {
    // Adds a new child element
    def add(child: T, end: Boolean) =
      children(child) = new TrieNode(new mutable.HashMap[T, TrieNode](), end)
      
    // Whether or not this node has a particular child
    def contains(child: T) = children.contains(child)
      
    // Gets a child node of this node
    def apply(child: T) = children(child)
    
    // Makes this node into a terminal node
    def makeTerminal = isTerminus = true
  }
  
  val children = new mutable.HashMap[T, TrieNode]()
  
  /**
   * Adds a string of child elements to the Trie.
   */
  def add(string: List[T]) {
    @tailrec
    def adder(node: TrieNode, string: List[T]) {
      string match {
        case Nil => node.makeTerminal
        case head :: tail => 
          if (!node.contains(head)) {
            node.add(head, false)
          }
          adder(node(head), tail)
      }
    }
    
    if (!children.contains(string.head)) {
      children(string.head) = new TrieNode(new mutable.HashMap[T, TrieNode](), false)
    }
    
    adder(children(string.head), string.tail)
  }
  
  /**
   * Tests whether or not a string is a member of the trie.
   * 
   * Note that this method can be used two ways:
   *  - If allowPrefix is false (the default), this method will return whether
   *    the Trie contains the string as a valid word.
   *  - If allowPrefix is true, this method will return true as long as the
   *    Trie contains the string, even if it isn't a complete word.
   */
  def contains(string: List[T], allowPrefix: Boolean = false): Boolean = {
    @tailrec
    def containsTester(node: TrieNode, string: List[T]): Boolean = {
      string match {
        case Nil => 
          if (!allowPrefix) node.isTerminus
          else true
        case head :: tail =>
          if (!node.contains(head)) {
            false
          } else {
            containsTester(node(head), tail)
          }
      }
    }
    
    if (!children.contains(string.head)) {
      false
    } else {
      containsTester(children(string.head), string.tail)
    }
  }
}