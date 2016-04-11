package net.cyndeline.rlgraph.drawings.planar.orthogonal.representation.help.darts

/**
 * Represents the direction a dart points at in a drawing.
 *
 * Usage: import DartDirection._ to access method conversions.
 */
object DartDirection extends Enumeration {
  type DartDirection = Value
  val Left = Value("Left")
  val Right = Value("Right")
  val Up = Value("Up")
  val Down = Value("Down")

  implicit def value2DirectionValue(dir: Value) = new DirectionValue(dir)

  class DirectionValue(dir: Value) {

    def turn(angle: Int): DartDirection = dir match {
        case Left => leftTurn(angle)
        case Right => rightTurn(angle)
        case Up => upTurn(angle)
        case Down => downTurn(angle)
      }

    def opposite = dir match {
      case Left => Right
      case Right => Left
      case Up => Down
      case Down => Up
    }

    private def leftTurn(angle: Int) = angle match {
      case 1 => Up
      case 2 => Left
      case 3 => Down
      case 4 => Right
    }

    private def rightTurn(angle: Int) = angle match {
      case 1 => Down
      case 2 => Right
      case 3 => Up
      case 4 => Left
    }

    private def upTurn(angle: Int) = angle match {
      case 1 => Right
      case 2 => Up
      case 3 => Left
      case 4 => Down
    }

    private def downTurn(angle: Int) = angle match {
      case 1 => Left
      case 2 => Down
      case 3 => Right
      case 4 => Up
    }
  }



}