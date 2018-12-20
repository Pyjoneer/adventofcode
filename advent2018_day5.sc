val data = scala.io.Source.fromFile("input.txt").getLines().toList.head

// Day 5, Part 1
def react(polymers: List[Char]): List[Char] = {

  def canReact(reactant1: Char, reactant2: Char) =
    (reactant1.isLower && reactant2.isUpper && reactant1 == reactant2.toLower) ||
      (reactant1.isUpper && reactant2.isLower && reactant1 == reactant2.toUpper)

  def reduce(polymers: List[Char], intermediates: List[Char], isProduct: Boolean): List[Char] = polymers match {
    case x1 :: x2 :: xs if canReact(x1, x2) => reduce(xs, intermediates, false)
    case x1 :: x2 :: xs if !canReact(x1, x2) => reduce(x2 :: xs, x1 :: intermediates, isProduct)
    case x :: xs => reduce(xs, x :: intermediates, isProduct)
    case Nil if !isProduct => reduce(intermediates.reverse, List(), true)
    case Nil if isProduct => intermediates.reverse
  }

  reduce(polymers, List(), true)
}

react(data.toCharArray.toList).mkString.length

// Day 5, Part 2
('a' to 'z').zip('A' to 'Z')
  .map { evictPolymer =>
    val polymers = data.toCharArray.toList.filterNot(p => p == evictPolymer._1 || p == evictPolymer._2)
    (evictPolymer._1, react(polymers).length)
  }.minBy(_._2)
