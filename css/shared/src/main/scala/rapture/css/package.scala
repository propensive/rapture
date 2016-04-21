package rapture.css

import language.implicitConversions

object `package` {
  implicit def cssStringContext(sc: StringContext) = new CssStrings(sc)
}
