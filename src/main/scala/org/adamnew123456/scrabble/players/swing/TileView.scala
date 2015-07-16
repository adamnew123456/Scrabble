package org.adamnew123456.scrabble.players.swing

import java.awt._
import javax.swing._

/**
 * Tile modes are used to configure how a tile is drawn. They also carry
 * semantic information behind them:
 * 
 * - Active is for tiles that can be moved on or removed from the board.
 * - Selected is for tiles which are about to be moved.
 * - Inactive is for tiles which cannot be moved or removed.
 */
trait TileMode {
  val background: Color
  
  /**
   * Configure draws the tile's background, and then sets the foreground color.
   */
  def configure(canvas: Graphics, dims: Dimension) = {
    canvas.setColor(background)
    canvas.fillRect(0, 0, dims.width, dims.height)
    
    canvas.setColor(Color.BLACK)
  }
}

case object Active extends TileMode {
  // This is an off-white color, called Bisque in HTML
  val background = new Color(0xFFE4C4)
}

case object Selected extends TileMode {
  // This is a light green color, called LightGreen in HTML
  val background = new Color(0x90EE90)
}

case object Inactive extends TileMode {
  // This is a grayish blue color, called LightSteelBlue in HTML
  val background = new Color(0xB0C4DE)
}

/**
 * TileView is responsible for drawing each tile on the screen. Being a
 * subclass of JPanel, it can also act like a button by having mouse
 * actions attached to it.
 */
class TileView(val tile: Char, val score: Int, var mode: TileMode) extends JPanel {
  val tileFont = new Font("Dialog", Font.PLAIN, 20)
  val pointFont = new Font("Dialog", Font.PLAIN, 14)
  
  setMinimumSize(new Dimension(40, 40))
  
  protected override def paintComponent(canvas: Graphics) {
    /*
     * +------+
     * | /-\  |
     * | \-|  |  g tile with a score of 5
     * |  -/  |
     * |5     |
     * +------+
     */
    val dims = getSize()
    mode.configure(canvas, dims)
    
    // The tile score goes at the bottom-left. Since the coordinate given is
    // used as the baseline of the font, it is drawn above the coordinate
    canvas.setFont(pointFont)
    
    val chars = score.toString.toArray
    canvas.drawChars(chars, 0, chars.length, 0, dims.height)
    canvas.setFont(tileFont)
    
    // The middle character is drawn as close to the center as we can get it.
    // Since AWT draws from the left edge, we have to shift the character over
    // to the left by half its width
    val metrics = getFontMetrics(tileFont)
    val fontWidth = metrics.charWidth(tile)
    val fontHeight = metrics.getHeight
    
    val xOffset = (dims.width / 2) - (fontWidth / 2)
    val yOffset = (dims.height / 2) + (fontHeight / 2)
    canvas.drawChars(tile.toString.toArray, 0, 1, xOffset, yOffset)
  }
}

/**
 * EmptyTileView is responsible for taking the place of a TileView, when no
 * tile needs to be displayed.
 */
class EmptyTileView extends JPanel {
  val bgColor = new Color(0x333333)
  setMinimumSize(new Dimension(40, 40))
  
  protected override def paintComponent(canvas: Graphics) {
    canvas.setColor(bgColor)
    
    val dims = getSize()
    canvas.fillRect(0, 0, dims.width, dims.height)
  }
}