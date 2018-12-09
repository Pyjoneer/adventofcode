val data = scala.io.Source.fromFile("input.txt").getLines().toList

val regex = "#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)".r

case class Claim(id: Int, x: Int, y: Int,  w: Int, h: Int)
case class Patch(claimId: Int, x: Int, y: Int)

val claims = data.map {
  case regex(id, x, y, w, h) => Claim(id.toInt, x.toInt, y.toInt, w.toInt, h.toInt)
}

val patches = for {
  claim <- claims
  x <- 0 until claim.w
  y <- 0 until claim.h
} yield Patch(claim.id, claim.x + x, claim.y + y)

// Day 3, Part 1
patches.groupBy(p => (p.x, p.y)).count(_._2.size > 1)

// Day 3, Part 2
val nooverlaps = patches.groupBy(p => (p.x, p.y)).filter(_._2.size == 1).map(_._2.map(_.claimId)).flatten.toSet
val overlaps = patches.groupBy(p => (p.x, p.y)).filter(_._2.size > 1).map(_._2.map(_.claimId)).flatten.toSet

nooverlaps -- overlaps
