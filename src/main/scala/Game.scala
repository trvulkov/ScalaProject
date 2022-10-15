package nine_mens_morris

import nine_mens_morris.enums.{Color, GameStatus, MoveType, Phase, Position}

import nine_mens_morris.Board
import cats.effect.{IO, IOApp}
import cats.data.State
import cats.syntax.all.*

import scala.io.StdIn.readLine

case class Game(
  board: Board = Board(),
  currentPlayer: Color = Color.White,
  phase: Phase = Phase.Placing()
):

  def status: GameStatus = GameStatus(
    phase,
    board.getPiecesOf(Color.White).size,
    board.getPiecesOf(Color.Black).size,
    board.canMoveAny(Color.White),
    board.canMoveAny(Color.Black)
  )

  def ask: String = phase match
    case Phase.Placing(i) => s"PLACING ($i pieces left total): $currentPlayer player (input one empty position):"
    case Phase.Moving if board.canFly(currentPlayer) =>
      s"MOVING (flying): $currentPlayer player (input two positions, separated by a space):"
    case Phase.Moving => s"MOVING: $currentPlayer player (input two adjacent positions, separated by a space):"
    case _ =>
      s"REMOVING ($currentPlayer formed a mill): $currentPlayer player (input one position, belonging to an opponent):"

  private def makeMove(move: MoveType): Either[String, (Board, MoveType)] = move match
    case MoveType.PlacementAt(position) => board.placePiece(currentPlayer, position).map((_, move))
    case MoveType.MovementBetween(from, to) => board.movePiece(currentPlayer, from, to).map((_, move))
    case MoveType.RemovalFrom(position) => board.removePiece(currentPlayer, position).map((_, move))

  private def processMoveResult(moveResult: (Board, MoveType)): (Board, Phase) =
    val (board, moveType) = moveResult
    moveType.getPiecePosition match
      case Some(position) if board.isInMill(currentPlayer, position) => (board, phase.toRemoval)
      case _ => (board, phase.next)

  private def update(nextBoard: Board, nextPhase: Phase): Game =
    if nextPhase.isRemoval then this.copy(board = nextBoard, phase = nextPhase) // currentPlayer stays the same
    else this.copy(board = nextBoard, currentPlayer = currentPlayer.other, phase = nextPhase)

  def process(move: MoveType): Either[String, Game] =
    makeMove(move).map(processMoveResult).map(update(_, _))

class GameRuntime:
  def readMove(prompt: String, phase: Phase): IO[MoveType] = for
    _ <- IO.println(prompt)
    line <- IO.readLine
    result <- MoveType(line, phase) match
      case Some(res) => res.pure[IO]
      case None => IO.println("ERROR: Invalid input!") >> readMove(prompt, phase)
  yield result

  def loop(game: Game): IO[Unit] = for
    _ <- IO.println(game.board)
    _ <- game.status match
      case GameStatus.InProgress =>
        for
          _ <- IO.println("")
          move <- readMove(game.ask, game.phase)
          result <- game.process(move) match
            case Right(nextGame) => loop(nextGame)
            case Left(error) => IO.println(error) >> loop(game)
        yield ()
      case end => IO.println(end)
  yield ()

object Main extends IOApp.Simple:
  def run: IO[Unit] = GameRuntime().loop(Game())
