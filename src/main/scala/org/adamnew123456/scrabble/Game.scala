package org.adamnew123456.scrabble

import scala.annotation.tailrec
import scala.collection.mutable.HashMap
import scala.util.{Try, Success, Failure}

/**
 * This is the final state of the game.
 */
case class EndGame(board: Board, scores: Map[String, Int], reason: EndReason)

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
  
  def runTurn(playerIndex: Int): Option[EndReason] = {
    val player = players(playerIndex)
    
    player.startTurn(board, playerTiles(player), namedScores)
    
    /**
     * Replacement can be invoked many times, if the player doesn't generate a
     * valid TileGroup to remove - this is why it is an inner @tailrec
     */
    @tailrec
    def doReplacement(redoReason: Option[Throwable]): Unit = {
      val currentTiles = playerTiles(player)
      val maxReplacement = Math.min(tileBag.tilesLeft, config.rackSize)
      val toReplace = player.replaceTiles(currentTiles, maxReplacement, redoReason)
      if (toReplace.size > 0) {
        val newTiles = for {
          remaining <- currentTiles.remove(toReplace)
          replacement <- tileBag.replaceTiles(toReplace)
          newTiles <- Success(remaining.merge(replacement))
        } yield newTiles
        
        newTiles match {
          case Success(newTiles) =>
            playerTiles(player) = newTiles
          case Failure(exn) => doReplacement(Some(exn))
        }
      }
    }
    
    if (tileBag.tilesLeft > 0) {
      doReplacement(None)
    }
    
    var gameEndReason: Option[EndReason] = None
    
    /**
     * Getting a valid move can also take multiple stages, if the player doesn't
     * generate a valid move on their first try.
     */
    @tailrec 
    def doMove(turnFailure: Option[TurnRejectReason]): Unit = {
      val playerMove = player.turn(board, playerTiles(player), turnFailure)
      val tilesUsed = TileGroup.fromTraversable(playerMove.values)
      
      var nextTurnFailure: Option[TurnRejectReason] = None
      val result = for {
        unusedTiles <- playerTiles(player).remove(tilesUsed)
        replacementTiles <- tileBag.drawTiles(config.rackSize - unusedTiles.size)
        newTiles <- Success(unusedTiles.merge(replacementTiles))
        
        newBoard <- board.addCharacters(playerMove)
        
        // Now, we just need to ensure that all the tiles added are somehow
        // connected to the center tile
        _ <- if (newBoard.isConnected) {
               Success(())
             } else {
               nextTurnFailure = Some(DisconnectReject)
               Failure(null)
             }
             
        // Note: We have to do the scoring section inside of the monad, since
        // it involves re-doing the turn if it finds a words that doesn't exist
        oldWords <- Success(board.findWords)
        newWords <- Success(newBoard.findWords)
        
        // If no words were added, then this player quits
        _ <- if (oldWords == newWords) { 
               gameEndReason = Some(ForfeitEnding(player.name))
               Success(()) // Not really a "success", but we don't need to ask
                           // the player for any more input
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
          
        case Failure(null) =>
          doMove(nextTurnFailure)
        case Failure(_: NoTilesError) =>
          // The game cannot continue if we lack tiles, but this doesn't mean
          // that we can try to run this turn again - there's no way it could
          // succeed
          gameEndReason = Some(EmptyBagEnding)
          ()
        case Failure(err) => 
          nextTurnFailure = Some(ExceptionReject(err))
          doMove(nextTurnFailure)
      }
    }
    
    doMove(None)
   
    gameEndReason match {
      case None => 
        player.endTurn(board, playerTiles(player), namedScores)
        None
      case _: Some[_] =>
        gameEndReason
    }
  }
  
  @tailrec
  final def run(player: Int = 0): EndGame = {
    runTurn(player) match {
      case None => run((player + 1) % players.length)
      case Some(reason) =>
        val game = EndGame(board, namedScores, reason)
        players.foreach(_.endGame(game))
        game
    }
  }
}
