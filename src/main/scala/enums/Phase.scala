package nine_mens_morris.enums

enum Phase:
  case Placing(piecesLeft: Int = 18)
  case Moving
  case RemovalAfterPlacement(piecesLeftToPlace: Int)
  case RemovalAfterMovement

  def next: Phase = this match
    case Placing(piecesLeft) if piecesLeft > 1 => Placing(piecesLeft - 1)
    case Placing(_) => Moving
    case Moving => Moving
    case RemovalAfterPlacement(piecesLeftToPlace) if piecesLeftToPlace > 0 => Placing(piecesLeftToPlace)
    case RemovalAfterPlacement(_) => Moving
    case RemovalAfterMovement => Moving

  def toRemoval: Phase = this match
    case Placing(piecesLeft) => RemovalAfterPlacement(piecesLeft - 1)
    case Moving => RemovalAfterMovement
    case s => s

  def isRemoval: Boolean = this match
    case RemovalAfterPlacement(_) => true
    case RemovalAfterMovement => true
    case _ => false
