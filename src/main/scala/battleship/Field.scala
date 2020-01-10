package battleship

import Game.Game.{MISS, Point, Shot}

class Field(private val size: Int, private val ships: List[Ship]) {

  validate(size, ships)

  private def validate(size: Int, ships: List[Ship]): Boolean = {

    // ships may not intersect
    val allShipCoordinates: List[Point] = {
      ships.foldRight(List.empty[Point])((s: Ship, l: List[Point]) => s.getCoordinates.toList ++ l)
    }

    // must be inside the field
    def isOutside(p: Point): Boolean = (p._1 >= 0) && (p._1 < size) && (p._2 >= 0) && (p._2 < size)
    val outside: List[Point] = allShipCoordinates filter isOutside

    (allShipCoordinates.size == allShipCoordinates.toSet.size) && outside.isEmpty
  }

  def play(guess: Point): (Shot, Field) = {
    val computed: List[(Shot, Ship)] = ships map { _.play(guess) }
    val result: Shot = computed find { case (MISS, _) => false; case _ => true } map(_._1) getOrElse MISS
    (result, new Field(size, computed map {_._2}))
  }

  def render: String = {
    val ocean = "~  "
    val ship = "o  "
    val hit = "x  "

    def drawPixel(point: Point) = ships
      .find { _.getCoordinates.contains(point) }
      .map { s: Ship => if (s.isHit(point)) hit else ship}
      .getOrElse(ocean)

    val pixels = for {
      x <- 0 until size
      y <- 0 until size
    } yield drawPixel((x, y))

    pixels.grouped(size).map(_.foldRight("\n")(_ + _)).foldRight("")(_ + _)
  }
}
