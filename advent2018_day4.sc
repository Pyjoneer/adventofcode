import java.time.format.DateTimeFormatter

val data = scala.io.Source.fromFile("C:/Users/Kazuhira/Desktop/input.txt").getLines().toList

val timeRegex = "\\[(.+)\\] (.+)".r
val beginShiftRegex = "Guard #(\\d+) begins shift".r
val fallAsleepRegex = "falls asleep".r
val wakeUpRegex = "wakes up".r

type Time = java.time.LocalDateTime

trait Event {
  def time: Time
}
case class BeginShift(guard: Int, time: Time) extends Event
case class FallAsleep(time: Time) extends Event
case class WakeUp(time: Time) extends Event

val events = data.map {
  case timeRegex(timestr, eventstr) =>
    val time = java.time.LocalDateTime.parse(timestr, DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm"))

    eventstr match {
      case beginShiftRegex(id) => BeginShift(id.toInt, time)
      case fallAsleepRegex() => FallAsleep(time)
      case wakeUpRegex() => WakeUp(time)
    }
}
.sortBy(_.time)(_.compareTo(_))


def makeShifts(events: List[Event], shift: List[Event] = List(), acc: List[List[Event]] = List()): List[List[Event]] = events match {
  case (x: BeginShift) :: xs => makeShifts(xs, List(x), shift :: acc)
  case x :: xs => makeShifts(xs, x :: shift, acc)
  case Nil => acc
}


val shifts = makeShifts(events, List(), List())

shifts