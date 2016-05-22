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

/* Includes the twenty most-traded currencies, as listed on Wikipedia, with the exception of Turkish Lira, because
   its ISO 4217 code is `TRY`, which is an inconvenient identifier. */
object currencies {
  object Gbp extends Currency("GBP", "Pound sterling", 2, "£", "") {
    type Key = Gbp
    implicit val currencyEvidence: Currency.Evidence[Gbp] = Currency.Evidence(this)
  }
  trait Gbp extends Currency.Key

  object Usd extends Currency("USD", "United States dollar", 2, "$", "") {
    type Key = Usd
    implicit val currencyEvidence: Currency.Evidence[Usd] = Currency.Evidence(this)
  }
  trait Usd extends Currency.Key

  object Eur extends Currency("EUR", "Euros", 2, "€", "") {
    type Key = Eur
    implicit val currencyEvidence: Currency.Evidence[Eur] = Currency.Evidence(this)
  }
  trait Eur extends Currency.Key
  
  object Jpy extends Currency("JPY", "Japanese yen", 2, "¥", "") {
    type Key = Jpy
    implicit val currencyEvidence: Currency.Evidence[Jpy] = Currency.Evidence(this)
  }
  trait Jpy extends Currency.Key
  
  object Aud extends Currency("AUD", "Australian dollar", 2, "$", "") {
    type Key = Aud
    implicit val currencyEvidence: Currency.Evidence[Aud] = Currency.Evidence(this)
  }
  trait Aud extends Currency.Key
  
  object Chf extends Currency("CHF", "Swiss franc", 2, "Fr", "") {
    type Key = Chf
    implicit val currencyEvidence: Currency.Evidence[Chf] = Currency.Evidence(this)
  }
  trait Chf extends Currency.Key
  
  object Cad extends Currency("CAD", "Canadian dollar", 2, "$", "") {
    type Key = Cad
    implicit val currencyEvidence: Currency.Evidence[Cad] = Currency.Evidence(this)
  }
  trait Cad extends Currency.Key
  
  object Mxn extends Currency("MXN", "Mexican peso", 2, "$", "") {
    type Key = Mxn
    implicit val currencyEvidence: Currency.Evidence[Mxn] = Currency.Evidence(this)
  }
  trait Mxn extends Currency.Key
  
  object Cny extends Currency("CNY", "Chinese yuan", 2, "¥", "") {
    type Key = Cny
    implicit val currencyEvidence: Currency.Evidence[Cny] = Currency.Evidence(this)
  }
  trait Cny extends Currency.Key
  
  object Nzd extends Currency("NZD", "New Zealand dollar", 2, "$", "") {
    type Key = Nzd
    implicit val currencyEvidence: Currency.Evidence[Nzd] = Currency.Evidence(this)
  }
  trait Nzd extends Currency.Key
  
  object Sek extends Currency("SEK", "Swedish krona", 2, "kr", "") {
    type Key = Sek
    implicit val currencyEvidence: Currency.Evidence[Sek] = Currency.Evidence(this)
  }
  trait Sek extends Currency.Key
  
  object Rub extends Currency("RUB", "Russian ruble", 2, "₽", "") {
    type Key = Rub
    implicit val currencyEvidence: Currency.Evidence[Rub] = Currency.Evidence(this)
  }
  trait Rub extends Currency.Key
  
  object Hkd extends Currency("HKD", "Hong Kong dollar", 2, "$", "") {
    type Key = Hkd
    implicit val currencyEvidence: Currency.Evidence[Hkd] = Currency.Evidence(this)
  }
  trait Hkd extends Currency.Key
  
  object Nok extends Currency("NOK", "Norwegian krone", 2, "kr", "") {
    type Key = Nok
    implicit val currencyEvidence: Currency.Evidence[Nok] = Currency.Evidence(this)
  }
  trait Nok extends Currency.Key
  
  object Sgd extends Currency("SGD", "Singapore dollar", 2, "$", "") {
    type Key = Sgd
    implicit val currencyEvidence: Currency.Evidence[Sgd] = Currency.Evidence(this)
  }
  trait Sgd extends Currency.Key
  
  object Krw extends Currency("KRW", "South Korean won", 2, "₩", "") {
    type Key = Krw
    implicit val currencyEvidence: Currency.Evidence[Krw] = Currency.Evidence(this)
  }
  trait Krw extends Currency.Key
  
  object Zar extends Currency("ZAR", "South African Rand", 2, "R", "") {
    type Key = Zar
    implicit val currencyEvidence: Currency.Evidence[Zar] = Currency.Evidence(this)
  }
  trait Zar extends Currency.Key
  
  object Brl extends Currency("BRL", "Brazilian real", 2, "R$", "") {
    type Key = Brl
    implicit val currencyEvidence: Currency.Evidence[Brl] = Currency.Evidence(this)
  }
  trait Brl extends Currency.Key
  
  object Inr extends Currency("INR", "Indian rupee", 2, "₹", "") {
    type Key = Inr
    implicit val currencyEvidence: Currency.Evidence[Inr] = Currency.Evidence(this)
  }
  trait Inr extends Currency.Key
  
}

