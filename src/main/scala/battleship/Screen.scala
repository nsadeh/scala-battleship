package battleship

import Game.Game.{MISS, Point, Shot}

class Screen(private val guesses: List[(Point, Shot)], private val size: Int) {

  def play(play: (Point, Shot)) = new Screen(play :: guesses, size)

  def render: String = {
    val unknown = "?  "
    val miss = "O  "
    val hit = "X  "

    def mkPixel(point: Point) = guesses
      .find { _._1 == point }
      .map { case (_, MISS) => miss; case _ => hit }
      .getOrElse(unknown)

    val pixels = for {
      x <- 0 until size
      y <- 0 until size
    } yield mkPixel((x, y))

    pixels.grouped(size).map(_.foldRight("\n")(_ + _)).foldRight("")(_ + _)
  }

}
