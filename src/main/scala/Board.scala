package nine_mens_morris

import nine_mens_morris.enums.{Color, Direction, GameStatus, Position, State}
import nine_mens_morris.enums.Position.*
import nine_mens_morris.{Node, Neighbors}

extension (board: Map[Position, Node])
  def placeAt(color: Color, position: Position): Map[Position, Node] =
    board + (position -> board(position).occupy(color))
  def removeFrom(position: Position): Map[Position, Node] =
    board + (position -> board(position).unoccupy())

case class Board private (board: Map[Position, Node]):
  override def toString: String = s"""
        white player has pieces at: ${getPiecesOf(Color.White)}
        black player has pieces at: ${getPiecesOf(Color.Black)}
        7 ${board(A7)}-----------${board(D7)}-----------${board(G7)}
          |           |           |
        6 |   ${board(B6)}-------${board(D6)}-------${board(F6)}   |
          |   |       |       |   |
        5 |   |   ${board(C5)}---${board(D5)}---${board(E5)}   |   |
          |   |   |       |   |   |
        4 ${board(A4)}---${board(B4)}---${board(C4)}       ${board(E4)}---${board(F4)}---${board(G4)}
          |   |   |       |   |   |
        3 |   |   ${board(C3)}---${board(D3)}---${board(E3)}   |   |
          |   |       |       |   |
        2 |   ${board(B2)}-------${board(D2)}-------${board(F2)}   |
          |           |           |
        1 ${board(A1)}-----------${board(D1)}-----------${board(G1)}
          a   b   c   d   e   f   g
  """

  // utilities and checks
  def getPiecesOf(color: Color): List[Position] =
    board.filter((_, node) => node.state == State.Occupied(color)).keys.toList

  def canMoveFrom(position: Position): Boolean = board(position).state match
    case State.Empty => false
    case State.Occupied(color) => board(position).neighbors.toList.exists(board(_).isEmpty)

  def canMoveAny(color: Color): Boolean =
    canFly(color) || getPiecesOf(color).exists(canMoveFrom)

  def canFly(color: Color): Boolean =
    getPiecesOf(color).size <= 3

  def allInMills(color: Color): Boolean =
    getPiecesOf(color).forall(isInMill(color, _))

  // game logic
  def placePiece(color: Color, position: Position): Either[String, Board] =
    if board(position).nonEmpty then Left(s"ERROR: $position is already occupied!")
    else Right(Board(board.placeAt(color, position)))

  def removePiece(remover: Color, position: Position): Either[String, Board] =
    if board(position).isEmpty then Left(s"ERROR: $position has nothing to remove!")
    else if board(position).isOccupiedBy(remover) then Left(s"$position is occupied by your own piece!")
    else if !allInMills(remover.other) && isInMill(remover.other, position) then Left(s"$position is in a mill!")
    else Right(Board(board.removeFrom(position)))

  def movePiece(color: Color, from: Position, to: Position): Either[String, Board] =
    if board(from).isEmpty then Left(s"ERROR: $from has no piece to move!")
    else if !board(from).isOccupiedBy(color) then Left(s"ERROR: $from is occupied by the other player's piece!")
    else if board(to).nonEmpty then Left(s"ERROR: $to is already occupied!")
    else if !canFly(color) && !board(from).hasNeighbor(to) then Left(s"ERROR: cannot move from $from to $to!")
    else Right(Board(board.removeFrom(from).placeAt(color, to)))

  def isInMill(color: Color, position: Position): Boolean =
    val inMiddleOfHorizontal: Boolean =
      (board(position).getNeighbor(Direction.Left), board(position).getNeighbor(Direction.Right)) match
        case (Some(left), Some(right)) if board(left).isOccupiedBy(color) && board(right).isOccupiedBy(color) => true
        case _ => false

    val inMiddleOfVertical: Boolean =
      (board(position).getNeighbor(Direction.Up), board(position).getNeighbor(Direction.Down)) match
        case (Some(up), Some(down)) if board(up).isOccupiedBy(color) && board(down).isOccupiedBy(color) => true
        case _ => false

    val onEdgeOfMill: Boolean =
      def millInDirection(direction: Direction): Boolean =
        board(position).getNeighbor(direction) match
          case Some(neighbor) if board(neighbor).isOccupiedBy(color) =>
            board(neighbor).getNeighbor(direction) match
              case Some(next) if board(next).isOccupiedBy(color) => true
              case _ => false
          case _ => false

      Direction.values.exists(millInDirection)

    inMiddleOfHorizontal || inMiddleOfVertical || onEdgeOfMill

object Board:
  def apply(): Board =
    val values =
      for position <- Position.values
      yield (position, Node(position))

    Board(Map[Position, Node](values*))
