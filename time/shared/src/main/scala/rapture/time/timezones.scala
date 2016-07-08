package rapture.time

import rapture.io._
import rapture.fs._
import rapture.uri._
import rapture.codec._, encodings.`UTF-8`._

object TimezoneData {

  case class TzDb(rules: Vector[Rule], zones: Vector[Zone], links: Vector[Link])

  object Integer {
    def unapply(s: String): Option[Int] = try Some(s.toInt) catch { case e: Exception => None }
  }

  object Time {

    val Hm = "^(-?[0-2]?[0-9]):([0-9][0-9])(s|u)?$".r
    val Hms = "^(-?[0-2]?[0-9]):([0-9][0-9]):([0-9][0-9])(s|u)?$".r

    def unapply(s: String): Option[Option[Time]] = if(s == null) Some(None) else Some(Some(s.trim match {
      case Integer(i) => Time(i, 0, 0, None)
      case Hm(Integer(h), Integer(m), x) => Time(h, m, 0, Option(x).map(_.head))
      case Hms(Integer(h), Integer(m), Integer(s), x) => Time(h, m, s, Option(x).map(_.head))
    }))

  }

  case class Time(hour: Int, minute: Int, second: Int, s: Option[Char])

  object ToYear {
    def unapply(s: String): Option[ToYear] = s match {
      case "only" => Some(OnlyYear)
      case "max" => Some(MaxYear)
      case other => Some(YearOf(other.toInt))
    }
  }
  sealed trait ToYear
  case class YearOf(n: Int) extends ToYear
  case object OnlyYear extends ToYear
  case object MaxYear extends ToYear

  object DayOfWeek {
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
      case AfterDay(DayOfWeek(d), n) => After(d, n.toInt)
      case s => DayNumber(s.toInt)
    })
  }

  sealed trait Day
  case class DayNumber(n: Int) extends Day
  case class After(dayOfWeek: DayOfWeek, day: Int) extends Day
  case class Last(dayOfWeek: DayOfWeek) extends Day

  sealed trait TzData
  case class Rule(area: String, from: YearOf, to: ToYear, month: Month, day: Day, time: Time, time2: Time, letter: Char) extends TzData

  case class Zone(name: String, rules: Vector[ZoneRule]) extends TzData
  case class ZoneRule(gmtOffset: Time, area: Option[String], zone: String, year: Option[Int], month: Option[Month], day: Option[Day], time: Option[Time]) extends TzData
  case class Link(zone1: String, zone2: String) extends TzData
  val dataDir = uri"file:///home/jpretty/tzdata"

  def files = List("africa", "antarctica", "asia", "australasia", "backward", "backzone", "etcetera", "europe", "northamerica", "southamerica", "pacificnew")

  def readFiles() = files.flatMap { f =>
    (dataDir / f).input[String].flatMap(parse(_))
  }.foldLeft(TzDb(Vector(), Vector(), Vector())) {
    case (TzDb(rules, zones, links), rule: Rule) => TzDb(rules :+ rule, zones, links)
    case (TzDb(rules, zones, links), zone: Zone) => TzDb(rules, zones :+ zone, links)
    case (TzDb(rules, zones :+ zone, links), zr: ZoneRule) => TzDb(rules, zones :+ zone.copy(rules = zone.rules :+ zr), links)
    case (TzDb(rules, zones, links), lnk: Link) => TzDb(rules, zones, links :+ lnk)
  }

  val Comment = """^#.*$""".r
  val BlankLine = """^\s*(#.*)?$""".r
  val RuleMatch = """Rule\s+([^#\s]+)\s+([^#\s]+)\s+([^#\s]+)\s+-\s+([^#\s]+)\s+([^#\s]+)\s+([^#\s]+)\s+([^#\s]+)\s+([^#\s]+).*$""".r
  val ZoneMatch = """Zone\s+([^#\s]+)(.*)$""".r
  val ZoneRuleMatch = """\s+([^#\s]+)\s+([^#\s]+)\s+([^#\s]+)(\s+[^#\s]+)?(\s+[^#\s]+)?(\s+[^#\s]+)?(\s+[^#\s]+)?.*$""".r
  val LinkMatch = """Link\s+([^#\s]+)\s+([^#\s]+).*$""".r

  def parse(line: String): List[TzData] = line match {
    case Comment() | BlankLine(_) => Nil
    
    case RuleMatch(area, fromYear, ToYear(toYear), month, Day(day), Time(time), Time(time2), s) =>
      List(Rule(area, YearOf(fromYear.toInt), toYear, Month.parse(month).get, day.get, time.get, time2.get, s.head))
    
    case ZoneRuleMatch(Time(offset), area, zone, y, m, Day(day), Time(time)) =>
      val List(year, month) = List(y, m).map(Option(_).map(_.trim))
      List(ZoneRule(offset.get, if(area == "-") None else Some(area), zone, year.map(_.toInt), month.flatMap(Month.parse(_)), day, time))
    
    case ZoneMatch(name, ZoneRuleMatch(Time(offset), area, zone, y, m, Day(day), Time(time))) =>
      val List(year, month) = List(y, m).map(Option(_).map(_.trim))
      List(Zone(name, Vector(ZoneRule(offset.get, if(area == "-") None else Some(area), zone, year.map(_.toInt), month.flatMap(Month.parse(_)), day, time))))
    
    case LinkMatch(zone1, zone2) =>
      List(Link(zone1, zone2))
    
    case other =>
      println(other)
      Nil
  }

}
