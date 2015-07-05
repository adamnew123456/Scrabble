import org.adamnew123456.scrabble._
import org.adamnew123456.scrabble.players._

val board = Board.empty(15, 15)
val config = new FileConfig("bin/letter-dist.txt", "bin/letter-score.txt", "bin/words.txt")

print("Enter your name: ")
val humanPlayer = new TerminalPlayer(readLine, config)
val aiPlayer = new NaiveComputerPlayer("<Computer>", config)

val game = new Game(board, config, List(humanPlayer, aiPlayer))
game.run()
