package org.adamnew123456.scrabble.players.swing

import java.util.concurrent.BlockingQueue
import javax.swing.SwingUtilities

import org.adamnew123456.scrabble.{ BasePlayer, Board, Config, EndGame, TileGroup, TurnRejectReason }

/**
 * This adapts the BasePlayer interface to the UI thread - most of what this
 * does is schedule things to run in the UI thread.
 * 
 * The question arises, then - how exactly can the UI thread block the game 
 * thread, but not vice versa? The structure looks like this:
 * 
 *  Game Thread             |            UI Thread
 *  ===========             |            =========
 *                          |
 *   /-------------/  1. invokeLater    /-----------/
 *  / SwingPlayer /---------|--------->/ UIManager / 3. User interaction. This
 * /-------------/          |         /-----------/     could take an arbitrary
 *            ^             |               |           amount of time.
 *            |             |               |
 *            | 2. Blocking |               | 4. When done, write the return
 *            |    read     |               |    value for the given operation
 *            |             |               |
 *            |     /--------------\        v
 *            \-<---| Return Queue |-----<--/
 *                  \--------------/
 *                          |
 */
class SwingPlayer(name: String, config: Config, 
    uiManager: UIManager, returnQueue: BlockingQueue[UIMessage]) 
    extends BasePlayer(name, config) {
  def fail(exn: Throwable): Nothing = {
    SwingUtilities.invokeLater(new ClosureRunnable({
      uiManager.gameError(exn)
    }))
    
    throw exn
  }
  
  def replaceTiles(tiles: TileGroup, maxReplace: Int, failReason: Option[Throwable]): TileGroup = {
    /*
     * Why invokeAndWait, instead of invokeLater?
     *
     * In the end, it doesn't make any difference - waiting on the BlockingQueue
     * ensures that the player is finished with their move before this function
     * actually returns. So, if this method didn't wait on the call into the 
     * UIManager, it would just end up waiting on the queue anyway.
     * 
     * This communicates the intent better, I think.
     */
    SwingUtilities.invokeAndWait(new ClosureRunnable(
      uiManager.replaceTilesMode(tiles, maxReplace, failReason)
    ))
    
    returnQueue.take match {
      case TileReplaceMessage(tiles) => tiles
      case _ => 
        fail(new IllegalStateException("Expected TileReplaceMessage"))
    }
  }
  
  def turn(board: Board, tiles: TileGroup, failReason: Option[TurnRejectReason]): Map[(Int, Int), Char] = {
    SwingUtilities.invokeAndWait(new ClosureRunnable(
      uiManager.turnMode(board, tiles, failReason)
    ))
    
    returnQueue.take match {
      case TurnMessage(move) => move
      case _ =>
        fail(new IllegalStateException("Expected TurnMessage"))
    }
  }
  
  /**
   * Messages the UI thread, telling it that its turn has started.
   */
  def startTurn(board: Board, tiles: TileGroup, scores: Map[String, Int]): Unit =
    SwingUtilities.invokeAndWait(new ClosureRunnable(
      uiManager.startTurn(board, tiles, scores)
    ))
    
  /**
   * Messages the UI thread, telling it that its turn has ended.
   */
  def endTurn(board: Board, tiles: TileGroup, scores: Map[String, Int]): Unit =
    SwingUtilities.invokeAndWait(new ClosureRunnable(
      uiManager.endTurn(board, tiles, scores)
    ))
    
  /**
   * Messages the UI thread, telling it that the game is over.
   */
  def endGame(result: EndGame): Unit =
    SwingUtilities.invokeLater(new ClosureRunnable(
      uiManager.endGame(result)
    ))
}