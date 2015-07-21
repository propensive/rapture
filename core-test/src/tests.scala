package rapture.core.scalazResult.test

import rapture.core._
import rapture.core.scalazResult.ResultT
import rapture.test._
import scalaz._
import Scalaz._


object Tests extends TestSuite {

  case class AlphaException() extends Exception
  case class BetaException() extends Exception
  case class MiscException() extends Exception

  val `Checking isErrata with errata` = test {
    ResultT.errata(Option(AlphaException())).run.get.isErrata
  } returns true

  val `Checking isErrata with answer` = test {
    ResultT.answer(Option(1)).run.get.isErrata
  } returns false

  val `Checking isAnswer with errata` = test {
    ResultT.errata(Option(AlphaException())).run.get.isAnswer
  } returns false

  val `Checking isAnswer with answer` = test {
    ResultT.answer(Option(1)).run.get.isAnswer
  } returns true

  val `Checking isUnforeseen with answer` = test {
    ResultT.answer(Option(1)).run.get.isUnforeseen
  } returns false

  val `Checking isUnforeseen with errata` = test {
    ResultT.errata(Option(AlphaException())).run.get.isUnforeseen
  } returns false

  val `filter answer` = test {
    ResultT.answer(Option(1)) filter (_ == 1)
  } returns ResultT(Some(Answer(1)))

  val `withFilter errata monadic` = test {
    for {
      x <- Option(Answer(1)) |> ResultT.apply
      if x == 0
      y = x + 1
    } yield y
  } returns ResultT(Some(Errata(Nil)))

  val `withFilter answer monadic` = test {
    for {
      x <- Option(Answer(1)) |> ResultT.apply
      if x == 1
      y = x + 1
    } yield y
  } returns ResultT(Some(Answer(2)))

}

