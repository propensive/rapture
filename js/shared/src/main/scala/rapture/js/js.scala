package rapture.js

case class Js(content: String) {
  override def toString = s"""js${"\""*3}$content${"\""*3}"""
}
