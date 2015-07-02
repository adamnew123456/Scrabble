package org.adamnew123456.scrabble

import java.util.Scanner
import scala.collection.mutable.HashMap

/**
 * This class determines the representation of some of the game's static data.
 * 
 * - The distribution of letters that appear when drawing tiles
 * - The game's word list, represented as a trie for efficient search
 */
trait Config {
  val letterDistribution: Map[Char, Int]
  val letterScores: Map[Char, Int]
  val wordList: Trie[Char]
  val rackSize: Int
}

/**
 * This class implements Config by loading the data using resources inside
 * the JAR.
 * 
 * The letter distribution and point information are scored in a simple
 * column format:
 * 
 *    a   9
 *    b   2
 *    c   2
 *    
 * The word list file contains all the words, one per line (not necessarily
 * in any particular order):
 * 
 *  aardvark
 *  zombie
 *  ...
 */
class ResourceConfig extends Config {
  def loadResource(path: String) = getClass().getResourceAsStream(path)
  
  private def loadLetterDistribution: Map[Char, Int] = {
    val letterScanner = new Scanner(loadResource("/res/letter-dist.txt"))
    val dist = new HashMap[Char, Int]()
    
    while (letterScanner.hasNext) {
      val word = letterScanner.next
      val score = letterScanner.nextInt
      
      dist(word(0)) = score
    }
    
    dist.toMap
  }
  
  private def loadLetterScores: Map[Char, Int] = {
    val letterScanner = new Scanner(loadResource("/res/letter-score.txt"))
    val scores = new HashMap[Char, Int]()
    
    while (letterScanner.hasNext) {
      val word = letterScanner.next
      val score = letterScanner.nextInt
      
      scores(word(0)) = score
    }
    
    scores.toMap
  }
  
  private def loadWordList: Trie[Char] = {
    val wordScanner = new Scanner(loadResource("/res/words.txt"))
    val words = new Trie[Char]()
    
    while (wordScanner.hasNext) {
      words.add(wordScanner.next.toList)
    }
    
    words
  }
  
  val letterDistribution = loadLetterDistribution
  val letterScores = loadLetterScores
  val wordList = loadWordList
  val rackSize = 7
}