import org.adamnew123456.scrabble._
import org.adamnew123456.scrabble.players._

class PrintObserver extends GameObserver {
    var playerOldScore = 0

    def startTurn(board: Board, currentPlayer: String, players: Map[String, PlayerInfo]) {
        playerOldScore = players(currentPlayer).score
    }

    def endTurn(board: Board, currentPlayer: String, players: Map[String, PlayerInfo]) {
        println(s"----- Turn: $currentPlayer -----")
        println(board)

        println(s"$currentPlayer scored ${players(currentPlayer).score - playerOldScore}")
        players.map {
            case (player: String, info: PlayerInfo) =>
                println(s"$player: Score = ${info.score}. Rack = ${info.tiles.asList.mkString(" ")}")
        }
    }

    def endGame(result: EndGame) {
      println("***** Game Over *****")
      println(s"Reason: ${result.reason}")

      println("Final Scores")
      result.scores.foreach {
        case (player: String, score: Int) =>
          println(s" - $player: Score = $score")
      }
    }
}

val board = Board.empty(15, 15)
val config = new FileConfig("bin/letter-dist.txt", "bin/letter-score.txt", "bin/words.txt")

val aiPlayer = new NaiveComputerPlayer("<Computer 1>", config)
val aiPlayer2 = new NaiveComputerPlayer("<Computer 2>", config)

val game = new Game(board, config, List(aiPlayer, aiPlayer2))
game.attachObserver(new PrintObserver)

game.run()
