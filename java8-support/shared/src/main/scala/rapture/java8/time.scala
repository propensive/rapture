package rapture.java8

import java.time._
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeFormatter._

import rapture.core.{StringParser, StringSerializer}
import rapture.data.DataAst



private[java8] trait TimeSerializers {

  implicit final def localDateSerializer(implicit ast: DataAst): StringSerializer[LocalDate] = serializeLocalDate(ISO_LOCAL_DATE)
  implicit final def localDateTimeSerializer(implicit ast: DataAst): StringSerializer[LocalDateTime] = serializeLocalDateTime(ISO_LOCAL_DATE_TIME)
  implicit final def localTimeSerializer(implicit ast: DataAst): StringSerializer[LocalTime] = serializeLocalTime(ISO_LOCAL_TIME)
  implicit final def zonedDateTimeSerializer(implicit ast: DataAst): StringSerializer[ZonedDateTime] = serializeZonedDateTime(ISO_ZONED_DATE_TIME)
  implicit final def offsetDateTimeSerializer(implicit ast: DataAst): StringSerializer[OffsetDateTime] = serializeOffsetDateTime(ISO_OFFSET_DATE_TIME)
  implicit final def offsetTimeSerializer(implicit ast: DataAst): StringSerializer[OffsetTime] = serializeOffsetTime(ISO_OFFSET_TIME)

  final def serializeLocalDate(formatter: DateTimeFormatter)(implicit ast: DataAst): StringSerializer[LocalDate] = {
    StringSerializer(_.format(formatter))
  }

  final def serializeLocalDateTime(formatter: DateTimeFormatter)(implicit ast: DataAst): StringSerializer[LocalDateTime] = {
    StringSerializer(_.format(formatter))
  }

  final def serializeLocalTime(formatter: DateTimeFormatter)(implicit ast: DataAst): StringSerializer[LocalTime] = {
    StringSerializer(_.format(formatter))
  }

  final def serializeZonedDateTime(formatter: DateTimeFormatter)(implicit ast: DataAst): StringSerializer[ZonedDateTime] = {
    StringSerializer(_.format(formatter))
  }

  final def serializeOffsetDateTime(formatter: DateTimeFormatter)(implicit ast: DataAst): StringSerializer[OffsetDateTime] = {
    StringSerializer(_.format(formatter))
  }

  final def serializeOffsetTime(formatter: DateTimeFormatter)(implicit ast: DataAst): StringSerializer[OffsetTime] = {
    StringSerializer(_.format(formatter))
  }

}

private[java8] trait TimeStringParsers {

  implicit final def localDateStringParser(implicit ast: DataAst): StringParser[LocalDate] = parseLocalDate(ISO_LOCAL_DATE)
  implicit final def localDateTimeStringParser(implicit ast: DataAst): StringParser[LocalDateTime] = parseLocalDateTime(ISO_LOCAL_DATE_TIME)
  implicit final def localTimeStringParser(implicit ast: DataAst): StringParser[LocalTime] = parseLocalTime(ISO_LOCAL_TIME)
  implicit final def zonedDateTimeStringParser(implicit ast: DataAst): StringParser[ZonedDateTime] = parseZonedDateTime(ISO_ZONED_DATE_TIME)
  implicit final def offsetDateTimeStringParser(implicit ast: DataAst): StringParser[OffsetDateTime] = parseOffsetDateTime(ISO_OFFSET_DATE_TIME)
  implicit final def offsetTimeStringParser(implicit ast: DataAst): StringParser[OffsetTime] = parseOffsetTime(ISO_OFFSET_TIME)


  final def parseLocalDate(formatter: DateTimeFormatter)(implicit ast: DataAst): StringParser[LocalDate] = {
    StringParser(LocalDate.parse(_, formatter))
  }


  final def parseLocalDateTime(formatter: DateTimeFormatter)(implicit ast: DataAst): StringParser[LocalDateTime] = {
    StringParser(LocalDateTime.parse(_, formatter))
  }

  final def parseLocalTime(formatter: DateTimeFormatter)(implicit ast: DataAst): StringParser[LocalTime] = {
    StringParser(LocalTime.parse(_, formatter))
  }

  final def parseZonedDateTime(formatter: DateTimeFormatter)(implicit ast: DataAst): StringParser[ZonedDateTime] = {
    StringParser(ZonedDateTime.parse(_, formatter))
  }

  final def parseOffsetDateTime(formatter: DateTimeFormatter)(implicit ast: DataAst): StringParser[OffsetDateTime] = {
    StringParser(OffsetDateTime.parse(_, formatter))
  }

  final def parseOffsetTime(formatter: DateTimeFormatter)(implicit ast: DataAst): StringParser[OffsetTime] = {
    StringParser(OffsetTime.parse(_, formatter))
  }

}

object time extends TimeSerializers with TimeStringParsers
