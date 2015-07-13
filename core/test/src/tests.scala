package rapture.core.test

import rapture.core._
import rapture.test._

object Tests extends TestSuite {

  case class AlphaException() extends Exception
  case class BetaException() extends Exception
  case class MiscException() extends Exception

  def alpha(x: Int)(implicit mode: Mode[_]): mode.Wrap[Int, AlphaException] = mode.wrap {
    if(x == 0) mode.exception(AlphaException())
    else if(x == 1) throw MiscException()
    else 0
  }
  
  def beta(x: Int)(implicit mode: Mode[_]): mode.Wrap[Int, BetaException] = mode.wrap {
    if(x == 0) mode.exception(BetaException())
    else if(x == 1) throw MiscException()
    else 0
  }

  val `Successful Result` = test {
    import modes.returnResult._
    alpha(2)
  } returns Answer(0)

  val `Unforeseen Result` = test {
    import modes.returnResult._
    alpha(1)
  } returns Unforeseen(MiscException())

  val `Expected error Result` = test {
    import modes.returnResult._
    alpha(0)
  } satisfies { case Errata(_) => true case _ => false }

  val `FlatMapped Successful Result` = test {
    import modes.returnResult._
    for {
      a <- alpha(2)
      b <- beta(2)
    } yield a + b
  } returns Answer(0)

  val `FlatMapped first fails` = test {
    import modes.returnResult._
    for {
      a <- alpha(0)
      b <- beta(2)
    } yield a + b
  } satisfies (_.exceptions == Vector(AlphaException()))

  val `FlatMapped second fails` = test {
    import modes.returnResult._
    for {
      a <- alpha(2)
      b <- beta(0)
    } yield a + b
  } satisfies (_.exceptions == Vector(BetaException()))

  val `Resolving errata 1` = test {
    import modes.returnResult._
    val result = for(a <- alpha(2); b <- beta(0)) yield a + b
    result.resolve(
      each[AlphaException] { e => 10 },
      each[BetaException] { e => 20 }
    )
  } returns Answer(20)

  val `Resolving errata 2` = test {
    import modes.returnResult._
    val result = for(a <- alpha(0); b <- beta(2)) yield a + b
    result.resolve(
      each[AlphaException] { e => 10 },
      each[BetaException] { e => 20 }
    )
  } returns Answer(10)

  val `Catching success` = test {
    Result.catching[AlphaException] {
      "success"
    }
  } returns Answer("success")

  val `Catching failure` = test {
    Result.catching[AlphaException] {
      throw AlphaException()
    }
  } satisfies (_.exceptions == Vector(AlphaException()))
  
  val `Catching unforeseen` = test {
    Result.catching[AlphaException] {
      throw BetaException()
    }
  } returns Unforeseen(BetaException())
}

