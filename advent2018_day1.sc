import scala.collection.immutable.HashSet
import scala.io.Source

// Day 1, part 1
Source
        .fromFile("input.txt")
        .getLines
        .map(_.toInt)
        .foldLeft(0)((a, b) => a + b)

// Day 1, part 2
val lines = Source
        .fromFile("input.txt")
        .getLines
        .toList
        .map(_.toInt)

val data = Iterator.continually(lines).flatten

def partialSum(data: Iterator[Int], partialSums: HashSet[Int], sum: Int): Int = {
    if (partialSums.contains(sum)) sum
    else partialSum(data, partialSums + sum, sum + data.next())
}

partialSum(data, HashSet.empty, 0)



