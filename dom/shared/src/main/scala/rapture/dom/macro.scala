/******************************************************************************************************************\
* Rapture, version 2.0.0. Copyright 2010-2016 Jon Pretty, Propensive Ltd.                                          *
*                                                                                                                  *
* The primary distribution site is http://rapture.io/                                                              *
*                                                                                                                  *
* Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance   *
* with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.            *
*                                                                                                                  *
* Unless required by applicable law or agreed to in writing, software distributed under the License is distributed *
* on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License    *
* for the specific language governing permissions and limitations under the License.                               *
\******************************************************************************************************************/
package rapture.dom

import rapture.base._

import language.experimental.macros
import language.implicitConversions

import language.higherKinds

object DomMacros {
 
  // The macro is invoked primarily to provide meaningful errors 
  def reportElementErrorMacro[Child <: ElementType: c.WeakTypeTag, Att <: AttributeType: c.WeakTypeTag,
      Child2 <: ElementType: c.WeakTypeTag, Elem <: ElementType: c.WeakTypeTag,
      Att2 <: AttributeType: c.WeakTypeTag](c: WhiteboxContext)(value: c.Expr[DomNode[Child2, Elem, Att]]): c.Expr[Any] = {
      //c.Expr[Applicable[Child, Att2, DomNode]] = {

    import c.universe._
   
    if(weakTypeOf[Elem].weak_<:<(weakTypeOf[Child])) {
      reify { value.splice.asInstanceOf[Applicable[Child, Att2, DomNode]] }
    } else {
      val found = weakTypeOf[Elem].toString.split(" with ").map(_.split("\\.").last).to[Vector] match {
        case Vector(one) => one
        case init :+ last => init.mkString(", ")+" or "+last
      }
      val expected = weakTypeOf[Child].toString.split(" with ").map(_.split("\\.").last).to[Vector] match {
        case Vector(one) => one
        case init :+ last => init.mkString(", ")+" or "+last
      }
    
      c.abort(c.enclosingPosition, s"Attempted to nest a $found node in a position where only $expected nodes are "+
          "permitted")
    }
  }
  
  def reportElementError2Macro[Child <: ElementType: c.WeakTypeTag, Att <: AttributeType: c.WeakTypeTag,
      Child2 <: ElementType: c.WeakTypeTag, Elem <: ElementType: c.WeakTypeTag]
      (c: WhiteboxContext)(value: c.Expr[DomNode[Child2, Elem, Att]]):
      c.Expr[Element[_, Child, _]] = {

    import c.universe._
   
    if(weakTypeOf[Elem].weak_<:<(weakTypeOf[Child])) {
      reify { value.splice.asInstanceOf[Element[_ <: ElementType, Child, _ <: AttributeType]] }
    } else {
      val found = weakTypeOf[Elem].toString.split(" with ").map(_.split("\\.").last).to[Vector] match {
        case Vector(one) => one
        case init :+ last => init.mkString(", ")+" or "+last
      }
      val expected = weakTypeOf[Child].toString.split(" with ").map(_.split("\\.").last).to[Vector] match {
        case Vector(one) => one
        case init :+ last => init.mkString(", ")+" or "+last
      }
    
      c.abort(c.enclosingPosition, s"Attempted to nest a $found node in a position where only $expected nodes are "+
          "permitted")
    }
  }
}
