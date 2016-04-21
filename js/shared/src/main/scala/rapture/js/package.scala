package rapture.js

import language.implicitConversions

object `package` {
  implicit def jsStringContext(sc: StringContext) = new JsStrings(sc)
}
