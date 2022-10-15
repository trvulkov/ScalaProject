package nine_mens_morris.enums

import nine_mens_morris.Neighbors

enum Position:
  case A1, A4, A7,
    B2, B4, B6,
    C3, C4, C5,
    D1, D2, D3, D5, D6, D7,
    E3, E4, E5,
    F2, F4, F6,
    G1, G4, G7

  def neighbors: Neighbors = this match
    case A1 => Neighbors(up = Some(Position.A4), right = Some(Position.D1))
    case A4 => Neighbors(up = Some(Position.A7), down = Some(Position.A1), right = Some(Position.B4))
    case A7 => Neighbors(down = Some(Position.A4), right = Some(Position.D7))

    case B2 => Neighbors(up = Some(Position.B4), right = Some(Position.D2))
    case B4 =>
      Neighbors(up = Some(Position.B6), down = Some(Position.B2), left = Some(Position.A4), right = Some(Position.C4))
    case B6 => Neighbors(down = Some(Position.B4), right = Some(Position.D6))

    case C3 => Neighbors(up = Some(Position.C4), right = Some(Position.D3))
    case C4 => Neighbors(up = Some(Position.C5), down = Some(Position.C3), left = Some(Position.B4))
    case C5 => Neighbors(down = Some(Position.C4), right = Some(Position.D5))

    case D1 => Neighbors(up = Some(Position.D2), left = Some(Position.A1), right = Some(Position.G1))
    case D2 =>
      Neighbors(up = Some(Position.D3), down = Some(Position.D1), left = Some(Position.B2), right = Some(Position.F2))
    case D3 => Neighbors(down = Some(Position.D2), left = Some(Position.C3), right = Some(Position.E3))
    case D5 => Neighbors(up = Some(Position.D6), left = Some(Position.C5), right = Some(Position.E5))
    case D6 =>
      Neighbors(up = Some(Position.D7), down = Some(Position.D5), left = Some(Position.B6), right = Some(Position.F6))
    case D7 => Neighbors(down = Some(Position.D6), left = Some(Position.A7), right = Some(Position.G7))

    case E3 => Neighbors(up = Some(Position.E4), left = Some(Position.D3))
    case E4 => Neighbors(up = Some(Position.E5), down = Some(Position.E3), right = Some(Position.F4))
    case E5 => Neighbors(down = Some(Position.E4), left = Some(Position.D5))

    case F2 => Neighbors(up = Some(Position.F4), left = Some(Position.D2))
    case F4 =>
      Neighbors(up = Some(Position.F6), down = Some(Position.F2), left = Some(Position.E4), right = Some(Position.G4))
    case F6 => Neighbors(down = Some(Position.F4), left = Some(Position.D6))

    case G1 => Neighbors(up = Some(Position.G4), left = Some(Position.D1))
    case G4 => Neighbors(up = Some(Position.G7), down = Some(Position.G1), left = Some(Position.F4))
    case G7 => Neighbors(down = Some(Position.G4), left = Some(Position.D7))

object Position:
  def fromString(str: String): Option[Position] =
    Position.values.find(_.toString == str.toUpperCase)
