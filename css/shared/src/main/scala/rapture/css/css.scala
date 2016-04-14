package rapture.css

case class Css(content: String) {
  override def toString = s"""css${"\""*3}$content${"\""*3}"""
}

case class CssStylesheet(content: String) {
  override def toString = s"""cssStylesheet${"\""*3}$content${"\""*3}"""
}
