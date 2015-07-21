import java.awt.BorderLayout
import java.awt.event._
import javax.swing._

import org.adamnew123456.scrabble._
import org.adamnew123456.scrabble.players.swing._

class RunClosure(closure: () => Unit) extends Runnable {
    def run = closure()
}

class ActionClosure(closure: ActionEvent => Unit) extends ActionListener {
    def actionPerformed(event: ActionEvent) = closure(event)
}

class TileModeEditor(callback: TileMode => Unit) extends JPanel {
  setLayout(new BoxLayout(this, BoxLayout.Y_AXIS))

  val group = new ButtonGroup()
  val List(makeActive, makeSelected, makeInactive) =
    List(Active, Selected, Inactive).zip(List("Active", "Selected", "Inactive")).map {
      case (mode, label) =>
        val button = new JRadioButton(label, false)
        button.addActionListener(new ActionClosure({ _ =>
          callback(mode)
        }))

        group.add(button)
        add(button)
        button
    }

  makeActive.setSelected(true)
}

SwingUtilities.invokeLater(new RunClosure({ () =>
    val main = new JFrame("TileView Test")
    main.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    main.getContentPane.setLayout(new BorderLayout())

    val buttonPanel = new JPanel()
    buttonPanel.setLayout(new BoxLayout(buttonPanel, BoxLayout.Y_AXIS))

    val tileView = new TileView('f', 5, Active)
    val tileModeEditor = new TileModeEditor({ mode: TileMode =>
      tileView.mode = mode
      tileView.repaint()
    })

    main.getContentPane.add(tileModeEditor, BorderLayout.EAST)
    main.getContentPane.add(tileView, BorderLayout.CENTER)

    main.pack()
    main.setVisible(true)
}))
