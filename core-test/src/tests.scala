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

  val `Checking isErrata with errata` = test {
    Result.errata(AlphaException()).isErrata
  } returns true

  val `Checking isErrata with answer` = test {
    Result.answer(1).isErrata
  } returns false

  val `Checking isAnswer with errata` = test {
    Result.errata(AlphaException()).isAnswer
  } returns false

  val `Checking isAnswer with answer` = test {
    Result.answer(1).isAnswer
  } returns true

  val `Checking isUnforeseen with answer` = test {
    Result.answer(1).isUnforeseen
  } returns false

  val `Checking isUnforeseen with errata` = test {
    Result.errata(AlphaException()).isUnforeseen
  } returns false

  val `Checking isUnforeseen with unforeseen` = test {
    Result.catching[AlphaException] {
      throw BetaException()
    }.isUnforeseen
  } returns true

  val `Fold answer` = test {
    Result.answer(1).fold(
      a => a + 1,
      e => 0
    )
  } returns 2

  val `Fold errata` = test {
    Result.errata[Int, AlphaException](AlphaException()).fold(
      a => a + 1,
      e => 0
    )
  } returns 0

  val `Exists answer` = test {
    Result.answer(1).exists(_ == 1)
  } returns true

  val `Exists answer none found` = test {
    Result.answer(1).exists(_ == 0)
  } returns false

  val `Exists errata` = test {
    Result.errata[Int, AlphaException](AlphaException()).exists(_ == 1)
  } returns false

  val `Forall answer` = test {
    Result.answer(1).forall(_ == 1)
  } returns true

  val `Forall answer none found` = test {
    Result.answer(1).forall(_ == 0)
  } returns false

  val `Forall errata` = test {
    Result.errata[Int, AlphaException](AlphaException()).forall(_ == 1)
  } returns true

  val `toList answer` = test {
    Result.answer(1).to[List]
  } returns List(1)

  val `toList errata` = test {
    Result.errata[Int, AlphaException](AlphaException()).to[List]
  } returns Nil

  val `toStream answer` = test {
    Result.answer(1).to[Stream]
  } returns Stream(1)

  val `toStream errata` = test {
    Result.errata[Int, AlphaException](AlphaException()).to[Stream]
  } returns Stream.empty[Int]

  val `toOption answer` = test {
    Result.answer(1).toOption
  } returns Some(1)

  val `toOption errata` = test {
    Result.errata[Int, AlphaException](AlphaException()).toOption
  } returns None

  val `toEither answer` = test {
    Result.answer(1).toEither
  } returns Right(1)

  val `toEither errata` = test {
    Result.errata[Int, AlphaException](AlphaException()).toEither
  } satisfies (v => v.isLeft)

  val `getOrElse answer` = test {
    Result.answer(1).getOrElse(0)
  } returns 1

  val `getOrElse errata` = test {
    Result.errata[Int, AlphaException](AlphaException()).getOrElse(0)
  } returns 0

  val `| answer` = test {
    Result.answer(1) | 0
  } returns 1

  val `| errata` = test {
    Result.errata[Int, AlphaException](AlphaException()) | 0
  } returns 0

  val `valueOr answer` = test {
    Result.answer(1).valueOr(_ => 0)
  } returns 1

  val `valueOr errata` = test {
    Result.errata[Int, AlphaException](AlphaException()).valueOr(_ => 0)
  } returns 0

  val `filter answer` = test {
    Result.answer(1) filter (_ == 1)
  } returns Answer(1)

  val `filter answer` = test {
    Result.answer(1) filter (_ == 0)
  } returns Errata(Nil)

  val `filter errata` = test {
    Errata[String, Nothing](Nil) filter (_.isEmpty)
  } returns Errata(Nil)

  val `withFilter errata monadic` = test {
    for {
      x <- Answer(1)
      if x == 0
      y = x + 1
    } yield y
  } returns Errata(Nil)

  val `withFilter answer monadic` = test {
    for {
      x <- Answer(1)
      if x == 1
      y = x + 1
    } yield y
  } returns Answer(2)

}

