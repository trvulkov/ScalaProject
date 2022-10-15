package nine_mens_morris.enums

enum Color:
  case White
  case Black

  def other: Color = this match
    case White => Black
    case Black => White
