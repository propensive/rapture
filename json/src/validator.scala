/******************************************************************************************************************\
* Rapture JSON, version 2.0.0. Copyright 2010-2015 Jon Pretty, Propensive Ltd.                                     *
*                                                                                                                  *
* The primary distribution site is http://rapture.io/                                                              *
*                                                                                                                  *
* Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in complance    *
* with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.            *
*                                                                                                                  *
* Unless required by applicable law or agreed to in writing, software distributed under the License is distributed *
* on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License    *
* for the specific language governing permissions and limitations under the License.                               *
\******************************************************************************************************************/
package rapture.json

import rapture.data._

private[json] object JsonValidator {

  case class ValidationException(strNo: Int, pos: Int, expected: String, found: Char)
      extends Exception
  
  case class DuplicateKeyException(strNo: Int, pos: Int, key: String) extends Exception

  def validate(parts: List[String]) = {
    var i = 0
    var n = 0
    def s = parts(n)
    def cur = if(i >= s.length) '\u0000' else s(i)

    def fail(expected: String) = throw ValidationException(n, i, expected, cur)
    def failPosition(expected: String) = throw ValidationException(n, i, expected, cur)
    def duplicateKey(start: Int, key: String) = throw DuplicateKeyException(n, start, key)
    
    def takeWhitespace(): Unit = while(cur.isWhitespace) next()

    def consume(cs: Char*): Unit = cs foreach { c => if(cur == c) next() else fail(s"'$c'") }

    def next() = i += 1

    def takeValue(): Unit = cur match {
      case '{' => takeObject()
      case '[' => takeArray()
      case '"' => takeString()
      case c if c.isDigit || c == '-' => takeNumber()
      case 't' => takeTrue()
      case 'f' => takeFalse()
      case 'n' => takeNull()
      case '\u0000' =>
        if(n + 1 < parts.length) {
          n += 1
          i = 0
        } else fail("new token or interpolated value")
      case _ => fail("new token")
    }

    def takeTrue() = consume('t', 'r', 'u', 'e')
    def takeFalse() = consume('f', 'a', 'l', 's', 'e')
    def takeNull() = consume('n', 'u', 'l', 'l')

    def takeNumber() = {
      if(cur == '-') next()
      
      if(cur == '0') next()
      else if(cur.isDigit) while(cur.isDigit) next()
      else fail("digit")
      
      if(cur == '.') {
        next()
        if(cur.isDigit) next() else fail("digit")
        while(cur.isDigit) next()
      }
      
      if(cur == 'e' || cur == 'E') {
        next()
        if(cur == '+' || cur == '-') next()
        if(cur.isDigit) next() else fail("digit")
        while(cur.isDigit) next()
      }
    }

    def takeObject(): Unit = {
      var seen: Set[String] = Set()
      def takeKeyValue(): Unit = {
        val start = i
        takeString()
        val key = s.substring(start + 1, i - 1)
        
        if(seen contains key) duplicateKey(start, key) else seen += key
        
        takeWhitespace()
        cur match {
          case ':' =>
            consume(':')
            takeWhitespace()
            takeValue()
            takeWhitespace()
            cur match {
              case ',' =>
                consume(',')
                takeWhitespace()
                takeKeyValue()
              case '}' => consume('}')
              case _ => fail("',' or '}'")
            }
          case _ => fail("':'")
        }
      }

      consume('{')
      takeWhitespace()
      cur match {
        case '"' => takeKeyValue()
        case '}' => consume('}')
        case _ => fail("'\"' or '}'")
      }
    }

    def takeArray(): Unit = {
      def takeElement(): Unit = {
        takeValue()
        takeWhitespace()
        cur match {
          case ',' =>
            consume(',')
            takeWhitespace()
            takeElement()
          case ']' => consume(']')
          case _ => fail("',' or ']'")
        }
      }
      consume('[')
      takeWhitespace()
      cur match {
        case ']' => consume(']')
        case _ => takeElement()
      }
    }

    def takeString(): Unit = {
      consume('"')
      while(cur != '"') cur match {
        case '\\' =>
          consume('\\')
          cur match {
            case '"' | '\\' | '/' | 'b' | 'f' | 'n' | 'r' | 't' => next()
            case 'u' =>
              consume('u')
              1 to 4 foreach { j =>
                if(cur.isDigit || cur >= 'a' && cur <= 'f' || cur >= 'A' && cur <= 'F') next()
                else fail("hexadecimal digit")
              }
          }
        case '\u0000' => failPosition("'\"' or more string content")
        case _ => next()
      }
      consume('"')
    }

    takeWhitespace()
    takeValue()
    takeWhitespace()
    if(i != s.length) fail("end of data")
  }
}
