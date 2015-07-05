package org.adamnew123456.scrabble

import java.io.File
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
 * Helper methods which are capable of loading from generalized streams. Used
 * for loading from JAR resources as well as files on the filesystem.
 */
abstract class StreamConfig extends Config {
  protected def loadLetterDistribution(letterScanner: Scanner): Map[Char, Int] = {
    val dist = new HashMap[Char, Int]()
    
    while (letterScanner.hasNext) {
      val word = letterScanner.next
      val score = letterScanner.nextInt
      
      dist(word(0)) = score
    }
    
    dist.toMap
  }
  
  protected def loadLetterScores(letterScanner: Scanner): Map[Char, Int] = {
    val scores = new HashMap[Char, Int]()
    
    while (letterScanner.hasNext) {
      val word = letterScanner.next
      val score = letterScanner.nextInt
      
      scores(word(0)) = score
    }
    
    scores.toMap
  }

  protected def loadWordList(wordScanner: Scanner): Trie[Char] = {
    val words = new Trie[Char]()
    
    while (wordScanner.hasNext) {
      words.add(wordScanner.next.toList)
    }
    
    words
  }
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
class ResourceConfig extends StreamConfig {
  private def loadResource(path: String): Scanner = 
    new Scanner(getClass().getResourceAsStream(path))
  
  val letterDistribution = loadLetterDistribution(loadResource("/res/letter-dist.txt"))
  val letterScores = loadLetterScores(loadResource("/res/letter-score.txt"))
  val wordList = loadWordList(loadResource("/res/words.txt"))
  val rackSize = 7
}

/**
 * This class implements Config by searching for files on the filesystem.
 */
class FileConfig(letterDistFile: String, letterScoreFile: String, wordFile: String) extends StreamConfig {
  private def loadResource(path: String): Scanner =
    new Scanner(new File(path))

  val letterDistribution = loadLetterDistribution(loadResource(letterDistFile))
  val letterScores = loadLetterScores(loadResource(letterScoreFile))
  val wordList = loadWordList(loadResource(wordFile))
  val rackSize = 7
}
