package org.adamnew123456.scrabble.test

import scala.util.{Try, Success, Failure}

import org.adamnew123456.scrabble.{Direction, NoSuchWordError, Trie, Word, WordScorer}

import org.junit.Test
import junit.framework.TestCase
import org.junit.Assert._

/**
 * Ensures that the WordScorer works as intended.
 */

class WordScorerTest extends TestCase {
  val tileScores = Map('a' -> 1, 'b' -> 2, 'c' -> 3, 
                       'd' -> 4, 'e' -> 5, 'f' -> 6)
                       
  val validWords = new Trie[Char]
  val wordList = List("cafe", "face", "babe", "fed", "bed", "fad")
  val wordListScores = Map[String, Int](
    "cafe" -> (3 + 1 + 6 + 5),
    "face" -> (6 + 1 + 3 + 5),
    "babe" -> (2 + 1 + 2 + 5),
    "fed" -> (6 + 5 + 4),
    "bed" -> (2 + 5 + 4),
    "fad" -> (6 + 1 + 4))
    
  wordList.foreach {word =>
    validWords.add(word.toList)
  }
  
  val scorer = new WordScorer(tileScores, validWords)

  def stringToWord(str: String) =
    Word(str, Direction.Horizontal, (0, 0))
  
  /**
   * Ensures that the words in the word list are all valid
   */
  def testValidWords() {
    wordList.map(stringToWord).foreach(word => assertTrue(scorer.isValidWord(word)))
  }
  
  /**
   * Ensures that a few invalid words aren't valid.
   */
  def testInvalidWords() {
    List("not", "in", "the", "word", "list").map(stringToWord).foreach {word => 
      assertFalse(scorer.isValidWord(word))
    }
  }
  
  /**
   * Ensures that words in the word list all score correctly.
   */
  def testScoredWords() {
    wordListScores.foreach {
      case (word: String, score: Int) => 
        val realWord = stringToWord(word)
        assertEquals(scorer.scoreWord(realWord), score)
    }
  }
  
  /**
   * Ensures that the word scorer can detect the difference between two sets
   * of words.
   */
  def testTurn() {
    val beforeTurn = Set("cafe", "babe").map(stringToWord)
    val afterTurn = Set("cafe", "babe", "fed").map(stringToWord)
    
    assertEquals(scorer.computeModifiedWords(beforeTurn, afterTurn), 
                Set("fed").map(stringToWord))
  }
  
  /**
   * Ensures that the word scorer correctly scores modified words.
   */
  def testScorer() {
    val beforeTurn = Set("cafe", "babe").map(stringToWord)
    val afterTurn = Set("cafe", "babe", "fed").map(stringToWord)
    
    val diff = scorer.computeModifiedWords(beforeTurn, afterTurn)
    scorer.computeTurnScore(diff) match {
      case Success(score) => assertEquals(score, wordListScores("fed"))
      case Failure(exn) => fail(s"Unexpected exception: $exn")
    }
  }
  
  /**
   * Ensures that the scorer rejects invalid words.
   */
  def testScorerRejectsBadWords() {
    val beforeTurn = Set("cafe", "babe").map(stringToWord)
    val afterTurn = Set("cafe", "babe", "invalid").map(stringToWord)
    
    val diff = scorer.computeModifiedWords(beforeTurn, afterTurn)
    scorer.computeTurnScore(diff) match {
      case Success(_) => fail("Expected a NoSuchWordError")
      case Failure(err: NoSuchWordError) => ()
      case Failure(exn) => fail(s"Expected NoSuchWordError, got $exn instead")
    }
  }
}
