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

package rapture.html

import rapture.dom._

object Html5 {
  trait Html extends AttributeType
  trait Base extends AttributeType
  trait Link extends AttributeType
  trait Meta extends AttributeType
  trait Style extends AttributeType
  trait Script extends AttributeType
  trait Body extends AttributeType
  trait Blockquote extends AttributeType
  trait Ol extends AttributeType
  trait Li extends AttributeType
  trait A extends AttributeType
  trait Q extends AttributeType
  trait Time extends AttributeType
  trait Progress extends AttributeType
  trait Meter extends AttributeType
  trait Bdo extends AttributeType
  trait Edit extends AttributeType
  trait Img extends AttributeType
  trait Iframe extends AttributeType
  trait Embed extends AttributeType
  trait Object extends AttributeType
  trait Param extends AttributeType
  trait Video extends AttributeType
  trait Audio extends AttributeType
  trait Source extends AttributeType
  trait Canvas extends AttributeType
  trait Map extends AttributeType
  trait Area extends AttributeType
  trait Col extends AttributeType
  trait Td extends AttributeType
  trait Th extends AttributeType
  trait Form extends AttributeType
  trait Fieldset extends AttributeType
  trait Label extends AttributeType
  trait Input extends AttributeType
  trait Button extends AttributeType
  trait Select extends AttributeType
  trait Optgroup extends AttributeType
  trait Option extends AttributeType
  trait Textarea extends AttributeType
  trait Output extends AttributeType
  trait Details extends AttributeType
  trait Command extends AttributeType
  trait Bb extends AttributeType
  trait Menu extends AttributeType

  trait Global extends Html with Base with Link with Meta with Style with Script with Body with Blockquote with Ol
      with Li with A with Q with Time with Progress with Meter with Bdo with Edit with Img with Iframe with Embed
      with Object with Param with Video with Audio with Source with Canvas with Map with Area with Col with Td with
      Th with Form with Fieldset with Label with Input with Button with Select with Optgroup with Option with
      Textarea with Output with Details with Command with Bb with Menu
  
  trait Flow extends ElementType
  trait Metadata extends ElementType
  trait Top extends ElementType
  trait Definitions extends ElementType
  trait ListItems extends ElementType
  trait TableItems extends ElementType
  trait ColItems extends ElementType
  trait TrItems  extends ElementType
  trait TdItems extends ElementType
  trait OptionItems extends ElementType
  trait Sectioning extends Flow
  trait Heading extends Flow
  trait Interactive extends Flow
  trait Phrasing extends Flow
  trait Embedded extends Phrasing
  trait Text extends Phrasing
}

