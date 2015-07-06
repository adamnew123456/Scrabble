package org.adamnew123456.scrabble

import scala.util.{Try, Success, Failure}

/**
 * This is an error raised when an invalid word is scored.
 */
case class NoSuchWordError(word: String) extends Exception {
  override def toString = s"NoSuchWordError($word)"
}

/**
 * This is responsible for checking whether words are valid, and scoring
 * them if they are.
 */
class WordScorer(letterScores: Map[Char, Int], wordList: Trie[Char]) {
  /**
   * Checks to see if a word is a valid word, or not.
   */
  def isValidWord(word: Word): Boolean = wordList.contains(word.text.toList)
  
  /**
   * This computes the score of a word.
   */
  def scoreWord(word: Word): Int = word.text.map(letterScores(_)).sum
  
  /**
   * This computes the difference between two word lists, and returns a list
   * of words which should be scored.
   */
  def computeModifiedWords(beforeWords: Set[Word], afterWords: Set[Word]): Set[Word] =
    afterWords.diff(beforeWords)
  
  /**
   * This takes two different word lists, and figures out the words that have
   * been added; if all the added words are valid, then the score from those
   * words is returned.
   */
  def computeTurnScore(words: Set[Word]): Try[Int] = {
    val invalidWords = words.filter(!isValidWord(_)).toList
    invalidWords match {
      case invalid :: _ => Failure(NoSuchWordError(invalid.text))
      case Nil => 
        val score = words.toList.map(scoreWord(_)).sum
        Success(score)
    }
  }
}