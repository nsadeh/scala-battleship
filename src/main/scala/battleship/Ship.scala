package battleship

import Game.Game._

class Ship (private val coordinates: Vector[Point]) {

  if (!validate(coordinates)) {
    throw new Exception("Invalid coordinate vectors for ship!")
  }

  private var hits = coordinates map { _ => false }
  private var sunk = false

  private def validate(points: Vector[Point]): Boolean = {
    // all points must be adjacent (not diagonal)
    def isAdjacent(p1: Point, p2: Point) = (p1._1 - p2._1).abs + (p1._2 - p2._2).abs == 1
    def adjacent(ps: Vector[Point]): Boolean = {
      if (ps.size < 2) true else isAdjacent(ps.apply(0), ps.apply(1)) && adjacent(ps.tail)
    }

    // must be on same line
    def linear(ps: Vector[Point]): Boolean = (ps.map{ _._1}.toSet.size == 1) || (ps.map{ _._2}.toSet.size == 1)

    val sortedPoints = points.sorted
    (sortedPoints.length >= 2) && adjacent(sortedPoints) && linear(sortedPoints)
  }

  def play(guess: Point): (Shot, Ship) = {
    val index: Int = coordinates.indexOf(guess)
    val isHit: Boolean = index >= 0 && !hits.apply(0)
    if (isHit) {
      val newHits = hits.updated(index, true)
      val newSunk = newHits reduce { _ && _}
      val newShip = new Ship(coordinates, newHits, newSunk)
      if (newSunk) (SUNK, newShip) else (HIT, newShip)
    } else (MISS, this)
  }

  private def this(coordinates: Vector[Point], hits: Vector[Boolean], sunk: Boolean) {
    this(coordinates)
    this.hits = hits
    this.sunk = sunk
  }

  def isSunk: Boolean = sunk
  def getHits: Vector[Boolean] = hits
  def getCoordinates: Vector[Point] = coordinates
  def isHit(point: Point): Boolean = if (coordinates.indexOf(point) >= 0)
    hits.apply(coordinates.indexOf(point)) else false
}

object Ship {

  def apply(points: Point*): Ship = new Ship(points.toVector)

}

