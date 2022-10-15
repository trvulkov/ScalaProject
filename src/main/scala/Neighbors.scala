package nine_mens_morris

import nine_mens_morris.enums.{Position, Direction}

case class Neighbors(
  up: Option[Position] = None,
  down: Option[Position] = None,
  left: Option[Position] = None,
  right: Option[Position] = None
):
  val toList: List[Position] = List(left, up, right, down).flatten

  def hasNeighbor(position: Position): Boolean =
    toList.contains(position)

  def get(direction: Direction): Option[Position] = direction match
    case Direction.Up => up
    case Direction.Down => down
    case Direction.Left => left
    case Direction.Right => right
