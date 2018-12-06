// Day 2, Part 1
val lines = scala.io.Source.fromFile("C:/Users/Kazuhira/Desktop/input2.txt").getLines().toList

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
