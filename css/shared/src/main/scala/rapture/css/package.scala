package rapture.css

object `package` {
  implicit def cssStringContext(sc: StringContext) = new CssStrings(sc)
}
