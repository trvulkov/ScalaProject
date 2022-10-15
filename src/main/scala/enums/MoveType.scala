package nine_mens_morris.enums

import nine_mens_morris.enums.{Phase, Position}

enum MoveType:
  case PlacementAt(position: Position)
  case MovementBetween(from: Position, to: Position)
  case RemovalFrom(position: Position)

  // used to check if a mill has been formed after the execution of the move (invalid for removal, since no mills can occur in that case)
  // better name?
  def getPiecePosition: Option[Position] = this match
    case PlacementAt(position) => Some(position)
    case MovementBetween(_, to) => Some(to)
    case _ => None

object MoveType:
  def apply(str: String, phase: Phase): Option[MoveType] = phase match
    case Phase.Placing(_) => Position.fromString(str).map(PlacementAt(_))
    case Phase.RemovalAfterPlacement(_) => Position.fromString(str).map(RemovalFrom(_))
    case Phase.RemovalAfterMovement => Position.fromString(str).map(RemovalFrom(_))
    case Phase.Moving =>
      str.split(" +") match // " +" is regex
        case Array(pos1, pos2) =>
          (Position.fromString(pos1), Position.fromString(pos2)) match
            case (Some(from), Some(to)) => Some(MovementBetween(from, to))
            case _ => None
        case _ => None
