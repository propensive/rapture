/******************************************************************************************************************\
* Rapture, version 2.0.0. Copyright 2010-2016 Jon Pretty, Propensive Ltd.                                          *
*                                                                                                                  *
* The primary distribution site is http://rapture.io/                                                              *
*                                                                                                                  *
* Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance   *
* with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.            *
*                                                                                                                  *
* Unless required by applicable law or agreed to in writing, software distributed under the License is distributed *
* on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License    *
* for the specific language governing permissions and limitations under the License.                               *
\******************************************************************************************************************/
package rapture.time

import rapture.core._

import java.util.Calendar
import java.text.SimpleDateFormat

object dateFormats {
  object shortUs { implicit val implicitDateFormat = DateFormat("MM/dd/yy") }
  object shortEuropean { implicit val implicitDateFormat = DateFormat("dd/MM/yy") }
  object longUs { implicit val implicitDateFormat =  DateFormat("MMMM d, yyyy") }
  object longEuropean { implicit val implicitDateFormat = DateFormat("d MMMM yyyy") }
}

object timeFormats {
  object hms { implicit val implicitTimeFormat = TimeFormat("HH:mm:ss") }
  object hm { implicit val implicitTimeFormat = TimeFormat("HH:mm") }
  object alternative { implicit val implicitTimeFormat = TimeFormat("h.mma") }
}

object `package` {
  implicit class TimeEnrichedString(s: StringContext) {
    private val Matcher1 = "([0-9][0-9]):([0-9][0-9])".r
    private val Matcher2 = "([0-9][0-9]):([0-9][0-9]):([0-9][0-9])".r
    def t(args: Int*): Time = s.parts.head match {
      case Matcher1(h, m) => Time(h.toInt, m.toInt)
      case Matcher2(h, m, s) => Time(h.toInt, m.toInt, s.toInt)
    }
  }

  case class DateFormat(pattern: String) {
    def format(d: Date): String = {
      val c = Calendar.getInstance
      c.setTimeInMillis(d.toLong)
      new SimpleDateFormat(pattern).format(c.getTime)
    }
    
    def format(dt: DateTime): String = {
      val c = Calendar.getInstance
      c.setTimeInMillis(dt.toLong)
      new SimpleDateFormat(pattern).format(c.getTime)
    }
  }

  case class TimeFormat(pattern: String) {
    def format(dt: DateTime): String = {
      val c = Calendar.getInstance
      c.setTimeInMillis(dt.toLong)
      new SimpleDateFormat(pattern).format(c.getTime)
    }
  }

  implicit val dateOrder = new Ordering[Date] {
    def compare(d1: Date, d2: Date) = if(d1 < d2) -1 else if(d2 == d1) 0 else 1
  }

  implicit val dateTimeOrder = new Ordering[DateTime] {
    def compare(d1: DateTime, d2: DateTime) = if(d1 < d2) -1 else if(d2 == d1) 0 else 1
  }

  def now() = DateTime.unapply(System.currentTimeMillis).get

  object Date {
    def unapply(n: Long) = {
      val c = Calendar.getInstance
      c.setTimeInMillis(n)
      Some(Date(c.get(Calendar.YEAR), c.get(Calendar.MONTH) + 1, c.get(Calendar.DATE)))
    }
  }

  object DateTime {
    def unapply(n: Long) = {
      val Date(date) = n
      val c = Calendar.getInstance
      c.setTimeInMillis(n)
      
      Some(DateTime(date, c.get(Calendar.HOUR_OF_DAY), c.get(Calendar.MINUTE),
          c.get(Calendar.SECOND)))
    }
  }

  object Time {
    def apply(hours: Int): Time = Time(hours, 0, 0)
    def apply(hours: Int, minutes: Int): Time = Time(hours, minutes, 0)
  }

  case class Time(hours: Int, minutes: Int, seconds: Int)

  case class Date(year: Int, month: Int, day: Int) { date =>
  
    override def toString() =
      day+"-"+monthString(month)+"-"+year

    def +(n: Int) = Date.unapply(n + toLong).get
    def -(n: Int) = Date.unapply(toLong - n).get

    override def equals(that: Any): Boolean = that match {
      case that: Date => toLong == that.toLong
      case that: DateTime => toLong == that.toLong
      case _ => false
    }

    def toLong = {
      val c = Calendar.getInstance
      c.set(Calendar.YEAR, year)
      c.set(Calendar.MONTH, month - 1)
      c.set(Calendar.DATE, day)
      c.getTimeInMillis
    }
   
    def at(time: Time) = DateTime(date, time.hours, time.minutes, time.seconds)

    def at(hours: Int) = new {
      def h(minutes: Int) = new DateTime(date, hours, minutes, 0) {
        def m(seconds: Int) = DateTime(date, hours, minutes, seconds)
      }
    }
    
    def >(that: Date): Boolean = toLong > that.toLong
    def <(that: Date): Boolean = toLong < that.toLong
    def >=(that: Date): Boolean = toLong >= that.toLong
    def <=(that: Date): Boolean = toLong <= that.toLong

    def format(implicit dateFormat: DateFormat) = dateFormat.format(this)
  }

  case class DateTime(date: Date, hour: Int, minute: Int, second: Int) {
    
    def pad(n: Int) = if(n < 10) "0"+n else n

    def +(n: Int) = DateTime.unapply(n + toLong).get
    def -(n: Int) = DateTime.unapply(toLong - n).get
    def -(d: DateTime): Long = toLong - d.toLong

    override def toString() =
      date.toString+" "+pad(hour)+":"+pad(minute)+":"+pad(second)

    override def equals(that: Any): Boolean = that match {
      case that: Date => toLong == that.toLong
      case that: DateTime => toLong == that.toLong
      case _ => false
    }

    def >(that: DateTime): Boolean = toLong > that.toLong
    def <(that: DateTime): Boolean = toLong < that.toLong
    def >=(that: DateTime): Boolean = toLong >= that.toLong
    def <=(that: DateTime): Boolean = toLong <= that.toLong

    def toLong = {
      val c = Calendar.getInstance
      c.setTimeInMillis(0L)
      c.set(Calendar.YEAR, date.year)
      c.set(Calendar.MONTH, date.month - 1)
      c.set(Calendar.DATE, date.day)
      c.set(Calendar.HOUR, hour)
      c.set(Calendar.MINUTE, minute)
      c.set(Calendar.SECOND, second)
      c.getTimeInMillis
    }
    
    def format(implicit dateFormat: DateFormat, timeFormat: TimeFormat) =
      dateFormat.format(this)+" "+timeFormat.format(this)
  }

  case class Month(no: Int)

  def monthString(n: Int) = List("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
      "Oct", "Nov", "Dec")(n - 1)

  val Jan = Month(1)
  val Feb = Month(2)
  val Mar = Month(3)
  val Apr = Month(4)
  val May = Month(5)
  val Jun = Month(6)
  val Jul = Month(7)
  val Aug = Month(8)
  val Sep = Month(9)
  val Oct = Month(10)
  val Nov = Month(11)
  val Dec = Month(12)

  implicit class IntoMonth(d: Int) {
    def -(m: Month) = new IntoDay(m)
    
    class IntoDay(m: Month) {
      def -(y: Int) = Date(y, m.no, d)
    }
  }
}
