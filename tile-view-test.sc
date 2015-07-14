import java.awt._
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

SwingUtilities.invokeLater(new RunClosure({ () =>
    val main = new JFrame("TileView Test")
    main.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    main.getContentPane.setLayout(new BorderLayout())

    val buttonPanel = new JPanel()
    buttonPanel.setLayout(new BoxLayout(buttonPanel, BoxLayout.Y_AXIS))

    val tileView = new TileView('f', 5, Active)

    val modeGroup = new ButtonGroup()

    val activeButton = new JRadioButton("Active", true)
    activeButton.addActionListener(new ActionClosure({ _ =>
        tileView.mode = Active
        tileView.repaint()
    }))

    val selectedButton = new JRadioButton("Selected", false)
    selectedButton.addActionListener(new ActionClosure({ _ =>
        tileView.mode = Selected
        tileView.repaint()
    }))

    val inactiveButton = new JRadioButton("Inactive", false)
    inactiveButton.addActionListener(new ActionClosure({ _ =>
        tileView.mode = Inactive
        tileView.repaint()
    }))

    modeGroup.add(activeButton)
    modeGroup.add(selectedButton)
    modeGroup.add(inactiveButton)

    buttonPanel.add(activeButton)
    buttonPanel.add(selectedButton)
    buttonPanel.add(inactiveButton)

    main.getContentPane.add(buttonPanel, BorderLayout.EAST)
    main.getContentPane.add(tileView, BorderLayout.CENTER)

    main.pack()
    main.setVisible(true)
}))
