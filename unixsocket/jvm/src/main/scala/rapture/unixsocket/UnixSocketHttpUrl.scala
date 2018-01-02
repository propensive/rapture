package rapture.unixsocket

import rapture.core.ParseException

case class UnixSocketHttpUrl(socketFileName: String, queryString: String, queryParams: Option[String])

object UnixSocketHttpUrl {

  private val UnixSocketRegex = """unix:\/\/(\/[^:]+):(\/?[^\?]*)(\?[^\?]*)?""".r

  @throws[ParseException]
  def parse(unixSocketUrl: String): UnixSocketHttpUrl = {
    unixSocketUrl match {
      case UnixSocketRegex(unixSocket, queryUrl, queryParams) => UnixSocketHttpUrl(unixSocket, queryUrl, Option(queryParams))
      case _ => throw new ParseException(unixSocketUrl, "unix socket url")
    }
  }

}

