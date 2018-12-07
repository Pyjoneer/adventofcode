//val lines = scala.io.Source.fromFile("input.txt").getLines().toList

val data = List("#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2")

case class Point(x: Int, y: Int) {
  def +(point: Point): Point = new Point(x + point.x, y + point.y)
}

object Point {
  def apply(s: String): Point = s.split(",|x").map(_.toInt) match { case Array(x, y) => new Point(x, y)}
}

case class Claim(id: Int, offset: Point, dims: Point)

object Claim {
  def apply(s: String): Claim = {
    val Array(id, off, dim) = s.replaceAll("\\s", "").split(Array('#', '@', ':')).filterNot(_.isEmpty)
    new Claim(id.toInt, Point(off), Point(off) + Point(dim))
  }
}

data.map(Claim(_))
