/*
  Rapture, version 2.0.0. Copyright 2010-2016 Jon Pretty, Propensive Ltd.

  The primary distribution site is
  
    http://rapture.io/

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
  compliance with the License. You may obtain a copy of the License at
  
    http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the License is
  distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and limitations under the License.
 */

package rapture.currency

import rapture.core._

object Currency {

  trait Key
  case class Evidence[C <: Currency.Key](currency: Currency { type Key = C })

  object Arithmetic extends Arithmetic_1 {
    implicit def sameCurrencies[C <: Currency.Key]: Arithmetic[Money[C], Money[C]] { type Return = Money[C] } =
      new Arithmetic[Money[C], Money[C]] {
        type Return = Money[C]
        def add(left: Money[C], right: Money[C]): Money[C] = Money(left.currency, left.amount + right.amount)
      }

    implicit def currencyBaskets[C <: Currency.Key, C2 <: Currency.Key]
      : Arithmetic[CurrencyBasket[C], CurrencyBasket[C2]] { type Return = CurrencyBasket[C with C2] } =
      new Arithmetic[CurrencyBasket[C], CurrencyBasket[C2]] {
        type Return = CurrencyBasket[C with C2]
        def add(left: CurrencyBasket[C], right: CurrencyBasket[C2]): CurrencyBasket[C with C2] =
          CurrencyBasket[C with C2](left.amounts.foldLeft(right.amounts) {
            case (acc, (k, v)) =>
              acc.updated(k, acc.get(k).getOrElse(0.0) + v)
          })
      }

    implicit def currencyBasketAndMoney[C <: Currency.Key, C2 <: Currency.Key]
      : Arithmetic[CurrencyBasket[C], Money[C2]] { type Return = CurrencyBasket[C with C2] } =
      new Arithmetic[CurrencyBasket[C], Money[C2]] {
        type Return = CurrencyBasket[C with C2]
        def add(left: CurrencyBasket[C], right: Money[C2]): CurrencyBasket[C with C2] =
          CurrencyBasket[C with C2](
              left.amounts.updated(right.currency, left.amounts.get(right.currency).getOrElse(0.0) + right.amount))
      }

    implicit def moneyAndCurrencyBasket[C <: Currency.Key, C2 <: Currency.Key]
      : Arithmetic[Money[C], CurrencyBasket[C2]] { type Return = CurrencyBasket[C with C2] } =
      new Arithmetic[Money[C], CurrencyBasket[C2]] {
        type Return = CurrencyBasket[C with C2]
        def add(left: Money[C], right: CurrencyBasket[C2]): CurrencyBasket[C with C2] =
          CurrencyBasket[C with C2](
              right.amounts.updated(left.currency, right.amounts.get(left.currency).getOrElse(0.0) + left.amount))
      }
  }

  trait Arithmetic_1 {
    implicit def differentCurrencies[C <: Currency.Key, C2 <: Currency.Key]
      : Arithmetic[Money[C], Money[C2]] { type Return = CurrencyBasket[C with C2] } =
      new Arithmetic[Money[C], Money[C2]] {
        type Return = CurrencyBasket[C with C2]
        def add(left: Money[C], right: Money[C2]): CurrencyBasket[C with C2] =
          if (left.currency == right.currency)
            CurrencyBasket[C with C2](Map(left.currency -> (left.amount + right.amount)))
          else
            CurrencyBasket[C with C2](Map(left.currency -> left.amount, right.currency -> right.amount))
      }
  }

  trait Arithmetic[T1, T2] {
    type Return
    def add(left: T1, right: T2): Return
  }
}

case class Currency(code: String, name: String, decimalPlaces: Int, prefix: String, suffix: String) {
  type Key <: Currency.Key
  override def hashCode = code.hashCode
  override def toString = code
  override def equals(that: Any) = that match {
    case that: Currency if that.code == code => true
    case _ => false
  }
  def apply[N: Numeric](amount: N): Money[Key] = Money[Key](this, implicitly[Numeric[N]].toDouble(amount))
}

case class InvalidMoney(currency: Currency)
    extends Exception(s"this is not a valid money value of currency ${currency.code}")

object Money {
  implicit def moneySerializer[C <: Currency.Key]: StringSerializer[Money[C]] = new StringSerializer[Money[C]] {
    def serialize(m: Money[C]) = {
      implicit val df: DecimalFormat = DecimalPlaces(m.currency.decimalPlaces)
      s"${String(m.amount)}${m.currency.code}"
    }
  }

  implicit def moneyParser[C <: Currency.Key](
      implicit ev: Currency.Evidence[C]): StringParser[Money[C]] { type Throws = InvalidMoney } =
    new StringParser[Money[C]] {
      type Throws = InvalidMoney
      def parse(s: String, mode: Mode[_ <: MethodConstraint]): mode.Wrap[Money[C], InvalidMoney] = mode.wrap {
        if (s.substring(s.length - 3) == ev.currency.code) {
          try Money[C](ev.currency, s.dropRight(3).toDouble)
          catch { case e: Exception => throw InvalidMoney(ev.currency) }
        } else throw InvalidMoney(ev.currency)
      }
    }
}

case class Money[C <: Currency.Key](currency: Currency { type Key = C }, amount: Double) {

  def unary_- = Money[C](currency, -amount)

  def +[C2 <: Currency.Key](m: Money[C2])(
      implicit arithmetic: Currency.Arithmetic[Money[C], Money[C2]]): arithmetic.Return =
    arithmetic.add(this, m)

  def -[C2 <: Currency.Key](m: Money[C2])(
      implicit arithmetic: Currency.Arithmetic[Money[C], Money[C2]]): arithmetic.Return =
    arithmetic.add(this, -m)

  def *[N: Numeric](times: N): Money[C] = Money(currency, amount * implicitly[Numeric[N]].toDouble(times))

  def /[N: Numeric](divisor: N): Money[C] = *(1.0 / implicitly[Numeric[N]].toDouble(divisor))

  def split(divisor: Int): List[Money[C]] = if(divisor == 0) Nil else {
    val rounded = (this/divisor).round
    rounded :: (this - rounded).split(divisor - 1)
  }

  def round: Money[C] = {
    val multiplier = math.pow(10, currency.decimalPlaces)
    Money[C](currency, (amount*multiplier).round/multiplier)
  }

  def roundingError: Money[C] = this - round

  def <(m: Money[C]) = amount < m.amount
  def <=(m: Money[C]) = amount <= m.amount
  def >(m: Money[C]) = amount > m.amount
  def >=(m: Money[C]) = amount >= m.amount

  override def toString = {
    implicit val df: DecimalFormat = DecimalPlaces(currency.decimalPlaces)
    s"${currency.prefix}${String(amount)}${currency.suffix}"
  }

  override def equals(that: Any): Boolean = that match {
    case m: Money[_] => amount == m.amount && (currency == m.currency || amount == 0.0)
    case _ => false
  }

  override def hashCode: Int = if(amount == 0.0) 0 else super.hashCode

}

case class CurrencyBasket[C <: Currency.Key](amounts: Map[Currency, Double]) {

  def +[M](m: M)(implicit arithmetic: Currency.Arithmetic[CurrencyBasket[C], M]): arithmetic.Return =
    arithmetic.add(this, m)

  def unary_- = CurrencyBasket[C](amounts.mapValues(-_))

  def *[N: Numeric](times: N): CurrencyBasket[C] =
    CurrencyBasket(amounts.mapValues(_ * implicitly[Numeric[N]].toDouble(times)))

  def /[N: Numeric](divisor: N): CurrencyBasket[C] = *(1.0 / implicitly[Numeric[N]].toDouble(divisor))

  def apply[C2 >: C <: Currency.Key](implicit ev: Currency.Evidence[C2]): Money[C2] =
    Money[C2](ev.currency, amounts(ev.currency))

  override def toString =
    amounts.map {
      case (currency, value) =>
        implicit val df: DecimalFormat = DecimalPlaces(currency.decimalPlaces)
        s"${String(value)}${currency.code}"
    }.mkString(", ")
}
