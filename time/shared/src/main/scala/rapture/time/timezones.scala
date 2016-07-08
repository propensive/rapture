package rapture.time

import rapture.core._
import rapture.io._
import rapture.fs._
import rapture.uri._
import rapture.codec._, encodings.`UTF-8`._

object TimezoneData {

  case class TzDb(rules: Map[String, Vector[Rule]], zones: Vector[Zone], links: Vector[Link])

  object Integer {
    def unapply(s: String): Option[Int] = try Some(s.as[Int]) catch { case e: Exception => None }
  }

  object Time {

    val Hm = "^(-?[0-2]?[0-9]):([0-9][0-9])(s|u)?$".r
    val Hms = "^(-?[0-2]?[0-9]):([0-9][0-9]):([0-9][0-9])(s|u)?$".r

    def unapply(s: String): Option[Option[TzTime]] = if(s == null) Some(None) else Some(Some(s.trim match {
      case Integer(i) => TzTime(i, 0, 0, None)
      case Hm(Integer(h), Integer(m), x) => TzTime(h, m, 0, Option(x).map(_.head))
      case Hms(Integer(h), Integer(m), Integer(s), x) => TzTime(h, m, s, Option(x).map(_.head))
    }))

  }

  case class TzTime(hour: Int, minute: Int, second: Int, s: Option[Char]) {
    override def toString = List(hour, minute, second).map { i =>
      if(i < 10) s"0$i" else i.toString
    }.mkString("", ":", s.map(_.toString).getOrElse(""))
  }

  object ToYear {
    def unapply(s: String): Option[ToYear] = s match {
      case "only" => Some(OnlyYear)
      case "max" => Some(MaxYear)
      case other => Some(YearOf(other.as[Int]))
    }
  }
  sealed trait ToYear
  case class YearOf(year: Int) extends ToYear
  case object OnlyYear extends ToYear
  case object MaxYear extends ToYear

  object DayOfWeek {

    private val days = Array(Sun, Mon, Tue, Wed, Thu, Fri, Sat)

    def fromDate(day: Int, month: Month, year: Int): DayOfWeek = {
      val Y = if(month.no < 3) year - 1 else year
      val c = Y/100
      val y = Y%100
      val m = (month.no + 9)%12 + 1
      days((day + (2.6*m - 0.2).toInt + y + y/4 + c/4 - 2*c + 700)%7)
    }

    def unapply(s: String): Option[DayOfWeek] = s match {
      case "Mon" => Some(Mon)
      case "Tue" => Some(Tue)
      case "Wed" => Some(Wed)
      case "Thu" => Some(Thu)
      case "Fri" => Some(Fri)
      case "Sat" => Some(Sat)
      case "Sun" => Some(Sun)
      case _ => None
    }
  }

  sealed trait DayOfWeek
  case object Mon extends DayOfWeek
  case object Tue extends DayOfWeek
  case object Wed extends DayOfWeek
  case object Thu extends DayOfWeek
  case object Fri extends DayOfWeek
  case object Sat extends DayOfWeek
  case object Sun extends DayOfWeek

  object Day {

    val LastDay = "^last(...)$".r
    val AfterDay = "^(...)>=([0-9]+)$".r

    def unapply(s: String): Option[Option[Day]] = Some(Option(s).map(_.trim).map {
      case LastDay(DayOfWeek(d)) => Last(d)
      case AfterDay(DayOfWeek(d), n) => After(d, n.as[Int])
      case s => DayNumber(s.as[Int])
    })
    
    def exactDate(d: Day, month: Month, year: Int) = d match {
      case DayNumber(n) => n
      case After(wd, start) => (start to (start + 6)).find { d => DayOfWeek.fromDate(d, month, year) == wd }.get
      case Last(wd) =>
        val end = Month.daysInMonth(month, year)
        ((end - 6) to end).find { d => DayOfWeek.fromDate(d, month, year) == wd }.get

    }
  }

  sealed trait Day
  case class DayNumber(n: Int) extends Day
  case class After(dayOfWeek: DayOfWeek, day: Int) extends Day
  case class Last(dayOfWeek: DayOfWeek) extends Day

  sealed trait TzData
  case class Rule(area: String, from: YearOf, to: ToYear, month: Month, day: Day, transition: TzTime, save: TzTime, letter: Option[Char]) extends TzData

  case class Zone(name: String, rules: Vector[Schedule]) extends TzData {
    /*def byYear(year: Int): Option[Schedule] = rules.filter { r => r.to match {
      case OnlyYear => year == r.from.year
      case MaxYear => r.from.year <= year
      case YearOf(y) => r.from.year <= year && y >= year
    } }*/
  }
  case class Schedule(gmtOffset: TzTime, area: Option[String], zone: String, year: Option[Int], month: Option[Month], day: Option[Day], time: Option[TzTime]) extends TzData
  case class Link(zone1: String, zone2: String) extends TzData
  val dataDir = uri"file:///home/jpretty/tzdata"

  def files = List("africa", "antarctica", "asia", "australasia", "backward", "backzone", "etcetera", "europe", "northamerica", "southamerica", "pacificnew")

  def readFiles() = files.flatMap { f =>
    (dataDir / f).input[String].flatMap(parse(_))
  }.foldLeft(TzDb(Map(), Vector(), Vector())) {
    case (TzDb(rules, zones, links), rule: Rule) =>
      TzDb(rules.updated(rule.area, rules.get(rule.area).getOrElse(Vector()) :+ rule), zones, links)
    case (TzDb(rules, zones, links), zone: Zone) =>
      TzDb(rules, zones :+ zone, links)
    case (TzDb(rules, zones :+ zone, links), zr: Schedule) =>
      TzDb(rules, zones :+ zone.copy(rules = zone.rules :+ zr), links)
    case (TzDb(rules, zones, links), lnk: Link) =>
      TzDb(rules, zones, links :+ lnk)
  }

  val Comment = """^#.*$""".r
  val BlankLine = """^\s*(#.*)?$""".r
  val RuleMatch = """Rule\s+([^#\s]+)\s+([^#\s]+)\s+([^#\s]+)\s+-\s+([^#\s]+)\s+([^#\s]+)\s+([^#\s]+)\s+([^#\s]+)\s+([^#\s]+).*$""".r
  val ZoneMatch = """Zone\s+([^#\s]+)(.*)$""".r
  val ScheduleMatch = """\s+([^#\s]+)\s+([^#\s]+)\s+([^#\s]+)(\s+[^#\s]+)?(\s+[^#\s]+)?(\s+[^#\s]+)?(\s+[^#\s]+)?.*$""".r
  val LinkMatch = """Link\s+([^#\s]+)\s+([^#\s]+).*$""".r

  def parse(line: String): List[TzData] = line match {
    case Comment() | BlankLine(_) => Nil
    
    case RuleMatch(area, fromYear, ToYear(toYear), month, Day(day), Time(time), Time(time2), s) =>
      List(Rule(area, YearOf(fromYear.as[Int]), toYear, month.as[Month], day.get, time.get, time2.get, if(s == "-") None else s.headOption))
    
    case ScheduleMatch(Time(offset), area, zone, y, m, Day(day), Time(time)) =>
      val List(year, month) = List(y, m).map(Option(_).map(_.trim))
      List(Schedule(offset.get, if(area == "-") None else Some(area), zone, year.map(_.as[Int]), month.flatMap(_.as[Option[Month]]), day, time))
    
    case ZoneMatch(name, ScheduleMatch(Time(offset), area, zone, y, m, Day(day), Time(time))) =>
      val List(year, month) = List(y, m).map(Option(_).map(_.trim))
      List(Zone(name, Vector(Schedule(offset.get, if(area == "-") None else Some(area), zone, year.map(_.as[Int]), month.flatMap(_.as[Option[Month]]), day, time))))
    
    case LinkMatch(zone1, zone2) =>
      List(Link(zone1, zone2))
    
    case other =>
      println(other)
      Nil
  }

}
