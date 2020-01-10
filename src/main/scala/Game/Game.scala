package Game

import battleship.{Field, Screen, Ship}

import scala.io.StdIn

object Game extends App {

  type Point = (Int, Int)

  sealed trait Shot
  case object MISS extends Shot
  case object HIT extends Shot
  case object SUNK extends Shot

  println("Enter the field size: ")
  val size = StdIn readInt

  println("Enter the number of ships: ")
  val numShips = StdIn readInt



}
