package org.adamnew123456.scrabble.players.swing

import javax.swing.{ JScrollPane, JTable }
import javax.swing.table.AbstractTableModel

/**
 * This displays the scores of all the players.
 */
class ScoreView extends JScrollPane {
  val model = new AbstractTableModel {
    var currentScores = Map[String, Int]()
    // Best players are determined by score, in this case
    var bestPlayers: List[String] = Nil
   
    def getColumnCount = 2
    override def getColumnName(idx: Int) = idx match {
      case 0 => "Player"
      case 1 => "Score"
    }
    
    override def getRowCount = currentScores.size
      
    def getValueAt(row: Int, col: Int): Object = {
      val player = bestPlayers(row)
      col match {
        case 0 => player
        case 1 => Int.box(currentScores(player))
      }
    }
    
    def updateScores(scores: Map[String, Int]) {
      currentScores = scores
      bestPlayers = scores.keys.toList.sortWith(currentScores(_) > currentScores(_))
      fireTableDataChanged
    }
  }
  
  val table = new JTable(model)
  table.setFillsViewportHeight(true)
  setViewportView(table)
  
  /**
   * This updates the view with the latest scores from the game.
   */
  def update(scores: Map[String, Int]) = model.updateScores(scores)
}