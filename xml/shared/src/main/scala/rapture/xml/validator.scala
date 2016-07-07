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

package rapture.xml

private[xml] object XmlValidator {

  case class ValidationException(strNo: Int, pos: Int, expected: String, found: Char)
      extends Exception
  
  case class DuplicateKeyException(strNo: Int, pos: Int, key: String) extends Exception
  
  def validate(parts: List[String]) = {
    var i = 0
    var n = 0
    var stack: List[String] = Nil
    def s = parts(n)
    def cur = if(i >= s.length) '\u0000' else s(i)
    def ahead(j: Int) = if(i + j >= s.length) '\u0000' else s(i + j)

    def fail(expected: String) = throw ValidationException(n, i, expected, cur)
    def failPosition(expected: String) = throw ValidationException(n, i, expected, cur)
    def duplicateKey(start: Int, key: String) = throw DuplicateKeyException(n, start, key)
    
    def takeWhitespace(): Unit = while(cur.isWhitespace) next()

    def consume(cs: Char*): Unit = cs foreach { c => if(cur == c) next() else fail(s"'$c'") }

    def next() = i += 1

    def takeTag(): Unit = {
      consume('<')
      cur match {
        case '/' => takeEndTag()
        case '?' => takePi()
        case '!' => takeSpecial()
        case _ => takeStartTag(i)
      }
      if(stack.nonEmpty) takeText()
    }

    def takePi(): Unit = {
      consume('?')
      takeName()
      takeWhitespace()
      while(i < s.length && cur != '?') next()
      consume('?', '>')
    }

    def takeComment(): Unit = {
      consume("--": _*)
      while(i < s.length && !(cur == '-' && ahead(1) == '-')) next()
      consume("-->": _*)
    }

    def takeSpecial(): Unit = {
      consume('!')
      cur match {
        case '-' => takeComment()
        case '[' =>
          consume('[')
          cur match {
            case 'C' =>
              consume("CDATA[": _*)
            case 'P' =>
              consume("PCDATA[": _*)
            case _ =>
              fail("CDATA or PCDATA section")
          }
          while(i < s.length && !(cur == ']' && ahead(1) == ']')) next()
          consume("]]>": _*)
        case _ => fail("'-' or '['")
      }
    }

    def takeEndTag(): Unit = {
      consume('/')
      consume(stack.head: _*)
      stack = stack.tail
      consume('>')
    }

    def takeName(): String = {
      val start = i
      if(!cur.isLetter) fail("letter") else next()
      while(cur.isLetterOrDigit && i < s.length) next()
      s.substring(start, i)
    }

    def takeAttribute(): Unit = {
      takeName()
      takeWhitespace()
      consume('=')
      takeWhitespace()
      takeAttributeValue()
    }

    def takeStartTag(start: Int): Unit = {
      val tagName = takeName()
      stack ::= tagName
      takeWhitespace()
      takeTagContents(tagName)
    }

    def takeTagContents(tagName: String): Unit = cur match {
      case '>' =>
        consume('>')
      case '/' =>
        stack = stack.tail
        consume('/', '>')
      case _ =>
        takeAttribute()
        takeWhitespace()
        takeTagContents(tagName)
    }

    def takeText(): Unit = cur match {
      case '<' =>
        takeTag()
      case '&' =>
        takeEntity()
        takeText()
      case '\u0000' =>
        fail(s"""closing tag "${stack.head}"""")
      case _ =>
        next()
        takeText()
    }

    def takeEntity(): Unit = {
      consume('&')
      takeName()
      consume(';')
    }

    def takeAttributeValue(): Unit = cur match {
      case quot@('\'' | '"') =>
        consume(quot)
        var finished = false
        while(!finished) cur match {
          case `quot` =>
            consume(quot)
            finished = true
          case '&' =>
            takeEntity()
          case _ =>
            next()
        }
      case _ => fail("single or double quote")
    }
   
    takeTag()
    takeWhitespace()

    if(i != s.length) fail("end of data")
  }
}
