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

package rapture.http

trait Widgets {
  trait Widget
  
  case class Dropdown[T](options: List[T])(val id: T => String = ((t: Any) =>
      t.toString), val description: T => String = ((t: Any) => t.toString)) extends Widget
  
  case class RadioList[T](options: List[T])(val id: T => String = ((t: Any) => t.toString),
      val description: T => String = ((t: Any) => t.toString)) extends Widget
  
  case class TextArea(width: Int = 8, height: Int = 80, maxLength: Option[Int] = None) extends Widget
  case class HtmlEditor() extends Widget
  case class StringInput() extends Widget
  case class PasswordInput() extends Widget
  case class FileUploader() extends Widget
  case class Checkbox() extends Widget
  case class Hidden() extends Widget
}

