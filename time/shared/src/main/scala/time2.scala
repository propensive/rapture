package rapture.time2

trait Timezone

trait Calendar {
  
  class TimeUnit(magnitude: Int) { type Return }

  object TimeVector {
    def empty[T <: TimeUnit] = new TimeVector[T](Map())
  }

  class TimeVector[T <: TimeUnit](private[time2] val map: Map[TimeUnit, Any]) {
    def +[U <: TimeUnit](vector: Timevector[U]) = {
      
    }
  }

  class Event[T <: TimeUnit](val base: TimeVector[T], offset: TimeVector[T]) {
    def normalize: Event[T] = this
    def instant(implicit timezone: Timezone): Instant = new Instant()
    
    def get[U <: TimeUnit](value: U)(implicit ev: T <:< U): value.Return =
      normalize.base.map(value).asInstanceOf[value.Return]
  
    def +[V <: TimeUnit](p: Period[V]): Event[T with V] =
      new Event[T with V](base, offset + p.vector)
  }

  class Period[T](val vector: TimeVector[T])

  class Duration()
  
  class Instant()

}

object calendars {

  object gregorian extends Calendar {

    object Year extends TimeUnit(6) { type Return = Year }
    object Month extends TimeUnit(5) { type Return = Month }
    object Day extends TimeUnit(4) { type Return = Day }

    object Hour extends TimeUnit(3) { type Return = Hour }
    object Minute extends TimeUnit(2) { type Return = Minute }
    object Second extends TimeUnit(1) { type Return = Second }

    class Year(val year: Int) extends AnyVal
    
    class Month(val month: Int, val name: String) {
      override def toString = name
    }

    class Day(val day: Int) extends AnyVal
    class Hour(val hour: Int) extends AnyVal
    class Minute(val minute: Int) extends AnyVal
    class Second(val second: Int) extends AnyVal

    type DateType = Year.type with Month.type with Day.type

    class DayMonth(day: Int, month: Month) {
      def -(year: Int) = new Event[DateType](new TimeVector[DateType](Map(
        Year -> new Year(year),
        Month -> month,
        Day -> new Day(day)
      )), TimeVector.empty)
    }

    final val Jan = new Month(1, "Jan")
    final val Feb = new Month(2, "Feb")
    final val Mar = new Month(3, "Mar")
    final val Apr = new Month(4, "Apr")
    final val May = new Month(5, "May")
    final val Jun = new Month(6, "Jun")
    final val Jul = new Month(7, "Jul")
    final val Aug = new Month(8, "Aug")
    final val Sep = new Month(9, "Sep")
    final val Oct = new Month(10, "Oct")
    final val Nov = new Month(11, "Nov")
    final val Dec = new Month(12, "Dec")
    
    implicit class DayInt(day: Int) {
      def -(month: Month): DayMonth = new DayMonth(day, month)
    }

  }

}


object Testing {
  import calendars.gregorian._

  val birth = 15-Jan-1983

  println(birth.get(Day))
  println(birth.get(Month))
  println(birth.get(Year))



}
