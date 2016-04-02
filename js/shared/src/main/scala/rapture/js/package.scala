package rapture.js

object `package` {
  implicit def jsStringContext(sc: StringContext) = new JsStrings(sc)
}
