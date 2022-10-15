package nine_mens_morris

import nine_mens_morris.enums.{Color, Position, State, Direction}
import nine_mens_morris.Neighbors

case class Node(position: Position, state: State = State.Empty):
  val neighbors: Neighbors = position.neighbors

  def isEmpty: Boolean = state match
    case State.Empty => true
    case _ => false

  def nonEmpty: Boolean = !isEmpty

  def isOccupiedBy(color: Color): Boolean = state match
    case State.Occupied(c) if c == color => true
    case _ => false

  def occupy(color: Color): Node =
    this.copy(state = State.Occupied(color))

  def unoccupy(): Node =
    this.copy(state = State.Empty)

  def hasNeighbor(position: Position): Boolean = neighbors.hasNeighbor(position)

  def getNeighbor(direction: Direction): Option[Position] =
    neighbors.get(direction)

  override def toString: String = state match
    case State.Empty => "·"
    case State.Occupied(Color.White) => "○"
    case State.Occupied(Color.Black) => "●"
