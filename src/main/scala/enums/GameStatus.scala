package nine_mens_morris.enums

import nine_mens_morris.enums.{Color, Phase}

enum VictoryType:
  case EnemyTwoPieces
  case EnemyCantMove

enum GameStatus:
  case InProgress
  case Draw
  case Victory(color: Color, victoryType: VictoryType)

  override def toString: String = this match
    case InProgress => ""
    case Victory(Color.White, VictoryType.EnemyTwoPieces) => "WHITE PLAYER WINS - black player has less than 3 pieces!"
    case Victory(Color.Black, VictoryType.EnemyTwoPieces) => "BLACK PLAYER WINS - white player has less than 3 pieces!"
    case Victory(Color.White, VictoryType.EnemyCantMove) => "WHITE PLAYER WINS - black player cannot move any pieces!"
    case Victory(Color.Black, VictoryType.EnemyCantMove) => "BLACK PLAYER WINS - white player cannot move any pieces!"
    case Draw => "DRAW - neither player can move!"

object GameStatus:
  def apply(
    phase: Phase,
    whitePieces: Int,
    blackPieces: Int,
    whiteCanMove: Boolean,
    blackCanMove: Boolean
  ): GameStatus =
    phase match
      case Phase.Placing(_) => InProgress
      case Phase.RemovalAfterPlacement(_) => InProgress
      case Phase.RemovalAfterMovement => InProgress
      case Phase.Moving =>
        if whitePieces >= 3 && blackPieces >= 3 && whiteCanMove && blackCanMove then InProgress
        else if blackPieces < 3 then Victory(Color.White, VictoryType.EnemyTwoPieces)
        else if whitePieces < 3 then Victory(Color.Black, VictoryType.EnemyTwoPieces)
        else if whiteCanMove && !blackCanMove then Victory(Color.White, VictoryType.EnemyCantMove)
        else if !whiteCanMove && blackCanMove then Victory(Color.Black, VictoryType.EnemyCantMove)
        else Draw
