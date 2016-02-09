package rapture.cli.test

/*import rapture.core._
import rapture.cli._
import rapture.test._

import scala.util

class TestRun extends Programme {
  include(CliTests)
}

object CliTests extends TestSuite {

  import New._

  implicit object Captured extends SuggestionOutput {
    private var suggestions: Seq[Vector[String]] = Seq()
    def output(ss: Suggestions) = ss.output.foreach(suggestions = _)
    def last() = suggestions
  }

  val Alpha = Param[String]('a', 'alpha)
  val Beta = Param[Int]('b', 'beta)
  val Gamma = Param.flag('c', 'gamma)
  val Delta = Param[String]('d', 'delta)

  val alpha = ParamMap("-a", "alpha")
  val alphaIsBeta = ParamMap("-a", "beta")
  val alphaIsGamma = ParamMap("-a", "gamma")
  val beta = ParamMap("-b", "0")
  val gamma = ParamMap("-c")
  val delta = ParamMap("-d", "delta")
  val empty = ParamMap()

  val `Parse short flag` = test {
    ParamMap("-a").find("a").map(_.values.map(_()))
  } returns Some(Vector())

  val `Parse short param with value` = test {
    ParamMap("-a", "alpha").find("a").map(_.values.map(_()))
  } returns Some(Vector("alpha"))
  
  val `Parse multiple short params` = test {
    ParamMap("-abc").find("b").map(_.values.map(_()))
  } returns Some(Vector())
  
  val `Parse multiple short params with value` = test {
    ParamMap("-abc", "gamma").find("c").map(_.values.map(_()))
  } returns Some(Vector("gamma"))
  
  val `Parse long flag` = test {
    ParamMap("--alpha").find("alpha").map(_.values.map(_()))
  } returns Some(Vector())
  
  val `Parse long param` = test {
    ParamMap("--alpha", "value").find("alpha").map(_.values.map(_()))
  } returns Some(Vector("value"))
  
  val `Parse long param (multiple args)` = test {
    ParamMap("--alpha", "one", "two", "three").find("alpha").map(_.values.map(_()))
  } returns Some(Vector("one", "two", "three"))
  
  val `Parse last of multiple long flags` = test {
    ParamMap("--alpha", "--beta", "--gamma", "--delta").find("delta").map(_.values.map(_()))
  } returns Some(Vector())
  
  val `Parse first of multiple long flags` = test {
    ParamMap("--alpha", "--beta", "--gamma", "--delta").find("alpha").map(_.values.map(_()))
  } returns Some(Vector())
  
  val `Parse first of multiple long params` = test {
    ParamMap("--alpha", "one", "--beta", "--gamma",
        "--delta").find("alpha").map(_.values.map(_()))

  } returns Some(Vector("one"))
  
  val `Parse last of multiple long params` = test {
    ParamMap("--alpha", "one", "--beta", "--gamma", "--delta", "a", "b",
        "c").find("delta").map(_.values.map(_()))
  } returns Some(Vector("a", "b", "c"))
  
  val `Extract simple param` = test {
    Alpha.parse(ParamMap("-a", "alpha"))
  } returns "alpha"

  val `Extract int` = test {
    Beta.parse(ParamMap("-b", "1"))
  } returns 1

  val `Simple coproduct 1` = test {
    val parsed = (Alpha | Beta).parse(alpha)
    parsed.handle(
      Alpha by identity,
      Beta by { b => "beta" }
    )
  } returns "alpha"

  val `Simple coproduct 2` = test {
    val parsed = (Alpha | Beta).parse(beta)
    parsed.handle(
      Alpha by identity,
      Beta by { b => "beta" }
    )
  } returns "beta"

  val `Coproduct handler is total 1` = test {
    typeMismatch {
      val parsed = (Alpha | Beta).parse(beta)
      import deferTypeErrors._
      parsed.handle(
        Alpha by identity
      )
    }
  } returns true

  val `Coproduct handler is total 2` = test {
    typeMismatch {
      val parsed = (Alpha | Beta).parse(beta)
      import deferTypeErrors._
      parsed.handle(
        Beta by { b => "beta" },
        Alpha by identity
      )
    }
  } returns false
  
  val `Can't access invalid field` = test {
    typeMismatch {
      val parsed = (Alpha & Beta).parse(beta)
      import deferTypeErrors._
      parsed(Gamma)
    }
  } returns true
  
  //val `Refuse coproduct duplicates` = test {
  //  import modes.returnOption._
  //  (Alpha | Beta).parse(alpha ++ beta)
  //} returns None

  val `Simple product` = test {
    val parsed = (Alpha & Beta).parse(alpha ++ beta)
    parsed(Alpha) -> parsed(Beta)
  } returns ("alpha", 0)

  //val `Missing product value fails 1` = test {
  //  import modes.returnOption._
  //  (Alpha & Beta).parse(alpha)
  //} returns None
  
  //val `Missing product value fails 2` = test {
  //  import modes.returnOption._
  //  (Alpha & Beta).parse(beta)
  //} returns None
  
  //val `Missing product value fails 3` = test {
  //  import modes.returnOption._
  //  (Alpha & Beta).parse(empty)
  //} returns None

  val `Coproduct and product 1` = test {
    val parsed = (Alpha & Beta | Delta).parse(alpha ++ beta)
    parsed.handle(
      Alpha & Beta by { p => p(Alpha) -> p(Beta) },
      Delta by { d => ("delta", -1) }
    )
  } returns ("alpha", 0)
  
  val `Coproduct and product 2` = test {
    val parsed = (Alpha & Beta | Delta).parse(delta)
    parsed.handle(
      Alpha & Beta by { p => p(Alpha) -> p(Beta) },
      Delta by { d => ("delta", -1) }
    )
  } returns ("delta", -1)

  val `Coproduct and product failure 1` = test {
    import modes.returnOption._
    val parsed = (Alpha & Beta | Delta).parse(alpha)
  } returns None
  
  val `Coproduct and product failure 2` = test {
    import modes.returnOption._
    val parsed = (Alpha & Beta | Delta).parse(beta)
  } returns None

  //val `Coproduct and product failure 3` = test {
  //  import modes.returnOption._
  //  (Alpha & Beta | Delta).parse(alpha ++ delta)
  //} returns None
  
  //val `Coproduct and product failure 4` = test {
  //  import modes.returnOption._
  //  (Alpha & Beta | Delta).parse(beta ++ delta)
  //} returns None

  val `Optional value 1` = test {
    (~Alpha).parse(alpha)
  } returns Some("alpha")

  val `Optional value 2` = test {
    (~Alpha).parse(empty)
  } returns None
  
  val `Optional Product 1` = test {
    val parsed = (~Alpha & ~Beta).parse(empty)
    (parsed(~Alpha), parsed(~Beta))
  } returns (None, None)
  
  val `Optional Product 2` = test {
    val parsed = (~Alpha & ~Beta).parse(alpha)
    (parsed(~Alpha), parsed(~Beta))
  } returns (Some("alpha"), None)
  
  val `Optional Product 3` = test {
    val parsed = (~Alpha & ~Beta).parse(beta)
    (parsed(~Alpha), parsed(~Beta))
  } returns (None, Some(0))
  
  val `Optional Product 4` = test {
    val parsed = (~Alpha & ~Beta).parse(beta ++ alpha)
    (parsed(~Alpha), parsed(~Beta))
  } returns (Some("alpha"), Some(0))

  val `Complex extraction successes` = test {
    import modes.returnOption._
    
    val pattern = (Alpha & ~Beta | Beta & (Gamma | Delta))
    
    val successes = List(alpha, alpha ++ beta, beta ++ gamma, beta ++ delta)

    successes.map(pattern.parse(_)).forall(_.isDefined)
  } returns true

  val `Complex extraction failures` = test {
    import modes.returnOption._
    
    val pattern = (Alpha & ~Beta | Beta & (Gamma | Delta))
    
    val failures = List(beta, gamma, delta, beta ++ gamma ++ delta, alpha ++ gamma, alpha ++
        delta)

    failures.map(pattern.parse(_)).forall(_ == None)
  } returns true

  val `Neither or both` = test {
    import modes.returnOption._
    
    val pattern = ~(Alpha & Beta)
    
    val successes = List(alpha ++ beta, empty)
    val failures = List(alpha, beta, gamma)

    successes.map(pattern.parse(_)).forall(_.isDefined) &&
        failures.map(pattern.parse(_)).forall(_ == None)
  } returns true


  val `Check param value` = test {
    Alpha.of("alpha").parse(alpha)
  } returns "alpha"
  
  val `Check param value 2` = test {
    val parsed = (Alpha.of("beta") | Alpha.of("alpha")).parse(alpha)
    parsed.handle(
      Alpha by identity
    )
  } returns "alpha"
  
  val `Check param value 3` = test {
    import modes.returnOption._
    (Alpha.of("beta") | Alpha.of("gamma")).parse(alpha)
  } returns None
  
  val `Check param values combined` = test {
    import modes.returnOption._
    val pattern = Alpha.of("beta") & Beta | Alpha.of("gamma") & Gamma
    
    val successes = List(alphaIsGamma ++ gamma, alphaIsBeta ++ beta)
    val failures = List(alpha, beta, gamma, alphaIsGamma ++ beta, alphaIsBeta ++ gamma,
        alpha ++ beta, alpha ++ gamma)

    successes.map(pattern.parse(_)).forall(_.isDefined) &&
        failures.map(pattern.parse(_)).forall(_ == None)
  } returns true

  val `Check suggestions` = test {
    val Color = Param[String]('c', 'color).suggest("red", "green", "blue")
    
    Color.parse(ParamMap("--color", ""), 1)
    
    Captured.last()
  } returns Vector(Vector("red"), Vector("green"), Vector("blue"))
}
*/
