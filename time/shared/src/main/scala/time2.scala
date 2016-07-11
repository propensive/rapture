package rapture.time2

import rapture.core._

trait Timezone

case class BadDate()

abstract class TimeUnit[Return <: TimeValue](val magnitude: Int, val name: String) {
  def make(n: Int): Return
}

trait Calendar {
  
  object TimeVector {
    def empty[T <: TimeValue] = new TimeVector[T](Map())
  }

  class TimeVector[T <: TimeValue](private[time2] val map: Map[TimeUnit[_ <: TimeValue], Int]) {
    def +[U <: TimeValue](vector: TimeVector[U]): TimeVector[T with U] = new TimeVector(
      vector.map.foldLeft(map) { case (m, (k, v)) =>
        m + ((k, map.get(k).map(_ + v).getOrElse(v)))
      }
    )
  }

  class Event[B <: TimeValue](val base: TimeVector[_ >: B], val offset: TimeVector[_ >: B <: TimeValue]) {
    def normalize(implicit timeSpec: TimeSpec, mode: Mode[_]): mode.Wrap[Event[B], Exception] = mode.wrap {

      def doNormalize(acc: Vector[(TimeUnit[_ <: TimeValue], Int)], next: Vector[(TimeUnit[_ <: TimeValue], Int)]): Vector[(TimeUnit[_ <: TimeValue], Int)] = if(next.isEmpty) acc else {
        val (u, n) +: xs = next
        val max = timeSpec.maxValue(u, acc.toMap)
        if(max < n + acc.toMap.get(u).getOrElse(0)) doNormalize(acc.init :+ ((acc.last._1, acc.last._2 + 1)), (u, n - max) +: xs)
        else doNormalize(acc :+ ((u, acc.toMap.get(u).getOrElse(0) + n)), xs)
      }
      
      val newBase = doNormalize(base.map.to[Vector], offset.map.to[Vector].sortBy(-_._1.magnitude))
      
      new Event[B](new TimeVector(newBase.toMap), TimeVector.empty)
    }
    
    def instant(implicit timezone: Timezone): Instant = new Instant()
    
    def get[U <: TimeValue](implicit timeUnit: TimeUnit[U], ev: B <:< U, timeSpec: TimeSpec): U =
      timeUnit.make(normalize.base.map(timeUnit))
  
    def +[V <: TimeValue](p: Period[V]): Event[B with V] =
      new Event[B with V](base, offset + p.vector)

    def set[U <: TimeValue](u: U)(implicit timeUnit: TimeUnit[U]) =
      new Event[B with U](new TimeVector(base.map ++ Map(timeUnit -> u.value)), new TimeVector[U](offset.map + (timeUnit -> 0)))
  
    def +[U <: TimeValue](u: U)(implicit timeUnit: TimeUnit[U]) =
      new Event[B with U](base, offset + new TimeVector[U](Map(timeUnit -> u.value)))
  
    override def toString = {
      val params = base.map.to[Vector].sortBy(_._1.magnitude).map {
        case (k, v) => s"${k.name} = ${v}"
      }.mkString(", ")
      
      val params2 = offset.map.to[Vector].sortBy(_._1.magnitude).map {
        case (k, v) => s"${k.name} = ${v}"
      }.mkString(", ")
      s"Time($params + $params2)"
    }
  }

  class Period[T <: TimeValue](val vector: TimeVector[T])

  class Duration()
  
  class Instant()

}

trait TimeValue extends Any {
  def value: Int
  def +(v: Int): Int = value + v
}

trait TimeSpec {
  def maxValue(timeUnit: TimeUnit[_ <: TimeValue], current: Map[TimeUnit[_ <: TimeValue], Int]): Int
}

object calendars {

  object gregorian extends Calendar {

    implicit val timeSpec: TimeSpec = new TimeSpec {
      def maxValue(timeUnit: TimeUnit[_ <: TimeValue], current: Map[TimeUnit[_ <: TimeValue], Int]): Int = timeUnit match {
        case Year => Int.MaxValue
	      case Month => if(!current.contains(Year)) Int.MaxValue else 12
	      case Day => current.get(Month) match {
          case None => Int.MaxValue
          case Some(m) => m match {
            case 0 => 31
            case 1 => current.get(Year) match {
              case Some(y) => if(y%4 == 0 && y%100 != 0 || y%400 == 0) 29 else 28
              case None => 28
            }
            case 2 => 31
            case 3 => 30
            case 4 => 31
            case 5 => 30
            case 6 => 31
            case 7 => 31
            case 8 => 30
            case 9 => 31
            case 10 => 30
            case 11 => 31
          }
	      }
        case Hour => if(!current.contains(Day)) Int.MaxValue else 24
        case Minute => if(!current.contains(Hour)) Int.MaxValue else 60
        case Second => if(!current.contains(Minute)) Int.MaxValue else 60
        case Millisecond => if(!current.contains(Second)) Int.MaxValue else 1000
      }
    }

    implicit object Year extends TimeUnit[Year](6, "year") {
      def make(n: Int): Year = new Year(n)
    }
    implicit object Month extends TimeUnit[Month](5, "month") {
      def make(n: Int): Month = allMonths(n - 1)
    }
    implicit object Day extends TimeUnit[Day](4, "day") {
      def make(n: Int): Day = new Day(n)
    }

    implicit object Hour extends TimeUnit[Hour](3, "hour") {
      def make(n: Int): Hour = new Hour(n)
    }
    implicit object Minute extends TimeUnit[Minute](2, "minute") {
      def make(n: Int): Minute = new Minute(n)
    }
    implicit object Second extends TimeUnit[Second](1, "second") {
      def make(n: Int): Second = new Second(n)
    }
    
    object Millisecond extends TimeUnit[Millisecond](0, "millisecond") {
      def make(n: Int): Millisecond = new Millisecond(n)
    }

    class Year(val value: Int) extends AnyVal with TimeValue {
      override def toString = value.toString
    }
    
    class Month(val value: Int, val name: String) extends TimeValue {
      override def toString = name
    }

    class Day(val value: Int) extends AnyVal with TimeValue {
      override def toString = value.toString
    }
    class Hour(val value: Int) extends AnyVal with TimeValue {
      override def toString = value.toString
    }
    class Minute(val value: Int) extends AnyVal with TimeValue {
      override def toString = value.toString
    }
    class Second(val value: Int) extends AnyVal with TimeValue {
      override def toString = value.toString
    }
    class Millisecond(val value: Int) extends AnyVal with TimeValue {
      override def toString = value.toString
    }

    type DateType = Year with Month with Day

    class DayMonth(day: Int, month: Month) {
      def -(year: Int) = new Event[DateType](new TimeVector[DateType](Map(
        Year -> year,
        Month -> month.value,
        Day -> day
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
    
    private val allMonths = Vector(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)
    
    implicit class DayInt(day: Int) {
      def -(month: Month): DayMonth = new DayMonth(day, month)
    }

  }

}


object Testing {
  import calendars.gregorian._

  val birth = 15-Jan-1983

  println(birth.get[Day])
  println(birth.get[Month])
  println(birth.get[Year])



}
