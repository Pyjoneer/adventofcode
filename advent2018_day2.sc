val lines = scala.io.Source.fromFile("input.txt").getLines().toList

// Day 2, Part 1
def hashsum(boxes: List[String]): Int = {

  def groups(str: String): Map[Char, Int] = str.toCharArray.groupBy(it => it).map(gr => gr._1 -> gr._2.size)

  def loop(list: List[String], acc: List[Int]): Int = list match {
    case x :: xs => {
      val counts = groups(x)

      val nums: List[Int] = List(
        counts.find(it => it._2 == 2).map(it => 2),
        counts.find(it => it._2 == 3).map(it => 3)
      ).flatten

      loop(xs, acc ::: nums)
    }
    case Nil => acc.filter(_ == 2).size * acc.filter(_ == 3).size
  }

  loop(boxes, List())
}

hashsum(lines)

// Day 2, Part 2
def distance(word1: String, word2: String): Int = word1.zip(word2).toList.foldLeft(0) {
  (count, chars) => if (chars._1 != chars._2) count + 1 else count
}

def find(word: String, data: List[String]): Option[String] = data match {
  case x :: xs if distance(x, word) == 1 => Some(x)
  case x :: xs => find(word, xs)
  case Nil => None
}

val ret = for {
  word <- lines
} yield find(word, lines)

ret.flatten
