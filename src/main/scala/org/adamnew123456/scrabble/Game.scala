package org.adamnew123456.scrabble

import scala.annotation.tailrec
import scala.collection.mutable.HashMap
import scala.util.{Try, Success, Failure}

/**
 * This is the final state of the game.
 */
case class EndGame(board: Board, scores: Map[String, Int])

/**
 * Used when the player gives up.
 */
case object Forfeit extends Throwable

/**
 * This is the main manager class - it is responsible for:
 * 
 * - Running all the BasePlayer functions at the appropriate times
 * - Updating the Board on behalf of each BasePlayer
 * - Managing the tile racks of each BasePlayer
 * - Managing and updating the scores of each BasePlayer
 */
class Game(startingBoard: Board, config: Config, players: List[BasePlayer]) {
  val scorer = new WordScorer(config.letterScores, config.wordList)
  var board = startingBoard

  // The scorer has to be set here, since the code building each BasePlayer
  // doesn't have access to the scorer yet - we have to fill it in ourselves
  players.foreach(_.setScorer(scorer))
  
  // Ensure that each player's name is unique
  val uniqueNames = players.map(_.name).toSet
  if (uniqueNames.size < players.length) {
    throw new IllegalStateException(s"Cannot have players with duplicate names")
  }
  
  // Reset each player's score
  val scores = new HashMap[BasePlayer, Int]
  players.foreach { scores(_) = 0 }
  
  val tileBag = new TileBag(config.letterDistribution)
  
  // Ensure that each player gets a starting rack. If we can't get a rack, then
  // the game is over before it starts.
  val playerTiles = new HashMap[BasePlayer, TileGroup]
  players.foreach { player: BasePlayer =>
    val tiles = tileBag.drawTiles(config.rackSize) match {
      case Success(tiles) => tiles
      case Failure(exn)   => throw exn
    }
    
    playerTiles(player) = tiles
  }
  
  def namedScores: Map[String, Int] = scores.map { case (player, score) =>
    player.name -> score
  }.toMap
  
  def runTurn(playerIndex: Int): Boolean = {
    val player = players(playerIndex)
    
    player.startTurn(board, playerTiles(player), namedScores)
    
    /**
     * Replacement can be invoked many times, if the player doesn't generate a
     * valid TileGroup to remove - this is why it is an inner @tailrec
     */
    @tailrec
    def doReplacement: Unit = {
      val currentTiles = playerTiles(player)
      val maxReplacement = Math.min(tileBag.tilesLeft, config.rackSize)
      val toReplace = player.replaceTiles(currentTiles, maxReplacement)
      if (toReplace.size > 0) {
        val newTiles = for {
          remaining <- currentTiles.remove(toReplace)
          replacement <- tileBag.replaceTiles(toReplace)
          newTiles <- Success(remaining.merge(replacement))
        } yield newTiles
        
        newTiles match {
          case Success(newTiles) =>
            playerTiles(player) = newTiles
          case Failure(_) => doReplacement
        }
      }
    }
    
    if (tileBag.tilesLeft > 0) {
      doReplacement
    }
    
    // Scala doesn't like enabling @tailcall when a 'return' is part of a
    // pattern match - this kludges around that
    var shouldContinue = true
    
    /**
     * Getting a valid move can also take multiple stages, if the player doesn't
     * generate a valid move on their first try.
     */
    @tailrec 
    def doMove: Unit = {
      val playerMove = player.turn(board, playerTiles(player))
      val tilesUsed = TileGroup.fromTraversable(playerMove.values)
      
      val result = for {
        unusedTiles <- playerTiles(player).remove(tilesUsed)
        replacementTiles <- tileBag.drawTiles(config.rackSize - unusedTiles.size)
        newTiles <- Success(unusedTiles.merge(replacementTiles))
        
        newBoard <- board.addCharacters(playerMove)
        
        // Now, we just need to ensure that all the tiles added are somehow
        // connected to the center tile
        _ <- if (newBoard.isConnected) Success(())
             else {
               println("Failure: Board is not connected")
               Failure(null)
             }
             
        // Note: We have to do the scoring section inside of the monad, since
        // it involves re-doing the turn if it finds a words that doesn't exist
        oldWords <- Success(board.findWords)
        newWords <- Success(newBoard.findWords)
        
        // If no words were added, then this player quits
        _ <- if (oldWords == newWords) { 
               shouldContinue = false 
               println("Failure: No new words generated")
               Failure(Forfeit)
             }
             else Success(())
             
        addedWords <- Success(
          scorer.computeModifiedWords(oldWords, newWords))
            
         score <- scorer.computeTurnScore(addedWords)
      } yield (newTiles, newBoard, score)
      
      result match {
        case Success((newTiles, newBoard, score)) =>
          playerTiles(player) = newTiles
          board = newBoard
          scores(player) += score
          
        // If no tiles are left, we can't continue the game
        case Failure(_: NoTilesError) => shouldContinue = false
        case Failure(Forfeit)         => shouldContinue = false
        case Failure(err) => 
          println(s"[Move Error: ${player.name}] $err")
          doMove
      }
    }
    
    doMove
    
    // See the definition of shouldContinue for why this has to be there
    if (!shouldContinue) {
      return false
    }
    
    player.endTurn(board, playerTiles(player), namedScores)
    true
  }
  
  @tailrec
  final def run(player: Int = 0): EndGame = {
    if (runTurn(player)) {
      run((player + 1) % players.length)
    } else {
      val game = EndGame(board, namedScores)
      players.foreach {_.endGame(game)}
      game
    }
  }
}
