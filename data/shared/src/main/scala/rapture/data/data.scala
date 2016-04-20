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
package rapture.data

import rapture.core._

import scala.util.Try

import language.dynamics
import language.higherKinds
import language.existentials

@implicitNotFound("Cannot find an implicit Formatter for ${AstType} data.")
trait Formatter[-AstType <: DataAst] {
  type Out
  def format(any: Any): Out
}

object DataCompanion { object Empty }

trait DataCompanion[+Type <: DataType[Type, DataAst], -AstType <: DataAst] {

  type ParseMethodConstraint <: MethodConstraint

  def empty(implicit ast: AstType) =
    construct(MutableCell(ast.fromObject(Map())), Vector())

  def construct(any: MutableCell, path: Vector[Either[Int, String]])(implicit ast: AstType): Type

  def parse[Source: StringSerializer](s: Source)(implicit mode: Mode[ParseMethodConstraint],
      parser: Parser[Source, AstType]): mode.Wrap[Type, ParseException] = mode wrap {
    construct(try MutableCell(parser.parse(s).get) catch {
      case e: NoSuchElementException => mode.exception(ParseException(String(s)))
    }, Vector())(parser.ast)
  }

  def apply[T](t: T)(implicit ast: AstType, ser: Serializer[T, Type]): Type =
    construct(MutableCell(ser.serialize(t)), Vector())

  def raw(value: Any)(implicit ast: AstType): Type =
    construct(MutableCell(value), Vector())

  def format[T <: DataType[T, AstType]](data: T)(implicit f: Formatter[_ <: AstType]): f.Out =
    f.format(data.$normalize)

}

case class DynamicApplication[D](path: List[Either[Int, String]], application: ForcedConversion2[D])

case class DynamicPath[D](path: List[Either[Int, String]]) extends Dynamic {
  def selectDynamic(v: String) = DynamicPath[D](Right(v) :: path)
  def applyDynamic(v: String)(i: Int) = DynamicPath[D](Left(i) :: Right(v) :: path)
  def apply(i: Int) = DynamicPath[D](Left(i) :: path)

  def updateDynamic(p: String)(value: ForcedConversion2[D]) =
    DynamicApplication(Right(p) :: path, value)
  
  def update(i: Int, value: ForcedConversion2[D]) =
    DynamicApplication(Left(i) :: path, value)
}

case class MutableCell(var value: Any)

trait DynamicData[+T <: DynamicData[T, AstType], +AstType <: DataAst] extends Dynamic {

  /** Assumes the Json object wraps a `Map`, and extracts the element `key`. */
  def selectDynamic(key: String): T = $deref(Right(key) +: $path)

  def applyDynamic(key: String)(i: Int = 0): T = $deref(Left(i) +: Right(key) +: $path)

  def $deref($path: Vector[Either[Int, String]]): T
  def $path: Vector[Either[Int, String]]

}

object DataType {

  class DataClassOperations[T <: DataType[T, AstType], AstType <: DataAst](dataType: T) {
    def ++[S <: DataType[S, Rep] forSome { type Rep }](b: S): T = {
      val ast = dataType.$ast
      
      def merge(a: Any, b: Any): Any = {
        if(ast.isObject(a) && ast.isObject(b)) {
          ast.fromObject(ast.getKeys(b).foldLeft(ast.getObject(a)) { case (as, k) =>
            as + (k -> {
              if(as contains k) merge(as(k), ast.dereferenceObject(b, k)) else ast.dereferenceObject(b, k)
            })
          })
        } else if(ast.isArray(a) && ast.isArray(b)) ast.fromArray(ast.getArray(a) ++ ast.getArray(b))
        else b
      }

      val left = dataType.$normalize
      val right = if(ast != b.$ast) ast.convert(b.$normalize, b.$ast.asInstanceOf[DataAst]) else b.$normalize
      
      dataType.$wrap(merge(left, right), Vector())
    }

    def copy(pvs: (DynamicPath[T] => DynamicApplication[_ <: DataType[T, _ <: AstType]])*): T = {
      dataType.$wrap(pvs.foldLeft(dataType.$normalize) { case (cur, pv) =>
        
	val dPath = pv(DynamicPath(Nil))
	val ast = dataType.$ast
	
	if(dPath.application.nothing) cur else {
 
	  def nav(path: List[Either[Int, String]], dest: Any, v: Any): Any = path match {
            case Nil =>
	      v
            
	    case Right(next) :: list =>
	      val d = try ast.dereferenceObject(dest, next) catch { case e: Exception => ast.fromObject(Map()) }
	      val src = ast.getObject(if(ast.isObject(dest)) dest else Map())
	      ast.fromObject(src + ((next, nav(list, d, v))))

	    case Left(next) :: list =>
	      val d = try ast.dereferenceArray(dest, next) catch { case e: Exception => ast.fromArray(List()) }
              val src = if(ast.isArray(dest)) ast.getArray(dest) else Nil
              ast.fromArray(src.padTo(next + 1, ast.fromObject(Map())).updated(next, nav(list, d, v)))
	  }

	  nav(dPath.path.reverse, cur, dPath.application.value)
        }
      })
    }
  }
}

trait DataType[+T <: DataType[T, AstType], +AstType <: DataAst] {
  val $root: MutableCell
  implicit def $ast: AstType
  def $path: Vector[Either[Int, String]]
  def $normalize: Any = doNormalize(false)
  def $wrap(any: Any, $path: Vector[Either[Int, String]] = Vector()): T
  def $deref($path: Vector[Either[Int, String]] = Vector()): T
  def $extract($path: Vector[Either[Int, String]]): T

  def \(key: String): T = $deref(Right(key) +: $path)

  def \\(key: String): T = $wrap($ast.fromArray(derefRecursive(key, $normalize)))

  private def derefRecursive(key: String, any: Any): List[Any] =
    if(!$ast.isObject(any)) Nil else $ast.getKeys(any).to[List].flatMap {
      case k if k == key => List($ast.dereferenceObject(any, k))
      case k => derefRecursive(key, $ast.dereferenceObject(any, k))
    }

  protected def doNormalize(orEmpty: Boolean): Any = {
    yCombinator[(Any, Vector[Either[Int, String]]), Any] { fn => _ match {
      case (j, Vector()) => j: Any
      case (j, t :+ e) =>
        fn(({
          if(e.bimap(x => $ast.isArray(j), x => $ast.isObject(j))) {
            try e.bimap($ast.dereferenceArray(j, _), $ast.dereferenceObject(j, _)) catch {
              case TypeMismatchException(exp, fnd) => throw TypeMismatchException(exp, fnd)
              case e: Exception =>
                if(orEmpty) DataCompanion.Empty
                else throw MissingValueException()
            }
          } else throw TypeMismatchException(
            if($ast.isArray(j)) DataTypes.Array else DataTypes.Object,
                e.bimap(l => DataTypes.Array, r => DataTypes.Object)
          )
        }, t))
    } } ($root.value -> $path)
  }

  /** Assumes the Json object is wrapping a `T`, and casts (intelligently) to that type. */
  def as[S](implicit ext: Extractor[S, T], mode: Mode[`Data#as`]):
      mode.Wrap[S, ext.Throws] = ext.extract(this.asInstanceOf[T], $ast, mode)

  def is[S](implicit ext: Extractor[S, T]): Boolean = try {
    ext.extract(this.asInstanceOf[T], $ast, modes.throwExceptions())
    true
  } catch {
    case e: Exception => false
  }

  def apply(i: Int = 0): T = $deref(Left(i) +: $path)

  override def equals(any: Any) = try { any match {
    case any: DataType[_, _] => $normalize == any.$normalize
    case _ => false
  } } catch { case e: Exception => false }

  override def hashCode = $root.value.hashCode ^ 3271912

}

trait MutableDataType[+T <: DataType[T, AstType], AstType <: MutableDataAst]
    extends DataType[T, AstType] {

  def $updateParents(p: Vector[Either[Int, String]], newVal: Any): Unit =
    p match {
      case Vector() =>
        $root.value = newVal
      case Left(idx) +: init =>
        val jb = $deref(init)
        val newJb = $ast.setArrayValue(Try(jb.$normalize).getOrElse($ast.fromArray(Nil)), idx,
            newVal)
        
        if(jb match {
          case jb: AnyRef => newJb match {
            case newJb: AnyRef => jb ne newJb
            case _ => false
          }
          case jb => jb == newJb
        }) $updateParents(init, newJb)
      case Right(key) +: init =>
        val jb = $deref(init)
        val newJb = $ast.setObjectValue(Try(jb.$normalize).getOrElse($ast.fromObject(Map())),
            key, newVal)
        
        if(jb match {
          case jb: AnyRef => newJb match {
            case newJb: AnyRef => jb ne newJb
            case _ => false
          }
          case jb => jb == newJb
        }) $updateParents(init, newJb)
    }

  /** Updates the element `key` of the JSON object with the value `v` */
  def updateDynamic(key: String)(v: ForcedConversion2[T]): Unit =
    if(!v.nothing) $updateParents($path,
        $ast.setObjectValue(Try($normalize).getOrElse($ast.fromObject(Map())), key, v.value))

  /** Updates the `i`th element of the JSON array with the value `v` */
  def update[T2](i: Int, v: T2)(implicit ser: Serializer[T2, T]): Unit =
    $updateParents($path, $ast.setArrayValue(Try($normalize).getOrElse($ast.fromArray(Nil)), i,
        ser.serialize(v)))

  /** Removes the specified key from the JSON object */
  def -=(k: String): Unit = $updateParents($path, $ast.removeObjectValue(doNormalize(true), k))

  /** Adds the specified value to the JSON array */
  def +=[T2](v: T2)(implicit ser: Serializer[T2, T]): Unit = {
    val r = doNormalize(true)
    val insert = if(r == DataCompanion.Empty) $ast.fromArray(Nil) else r
    $updateParents($path, $ast.addArrayValue(insert, ser.serialize(v)))
  }
}

trait `Data#as` extends MethodConstraint
trait `Data#normalize` extends MethodConstraint

object ForcedConversion2 extends ForcedConversion2_1 {
  implicit def forceOptConversion[T, D](opt: Option[T])(implicit ser: Serializer[T, D]) =
    opt.map(t => ForcedConversion2[D](ser.serialize(t), false)) getOrElse
        ForcedConversion2[D](null, true)
}

trait ForcedConversion2_1 {
  implicit def forceConversion[T, D](t: T)(implicit ser: Serializer[T, D]) =
    ForcedConversion2[D](ser.serialize(t), false)
}

case class ForcedConversion2[-D](value: Any, nothing: Boolean)

object ForcedConversion extends ForcedConversion_1 {
  implicit def forceOptConversion[T, D](opt: Option[T])(implicit ser: Serializer[T, D]) =
    opt.map(t => ForcedConversion[D](ser.serialize(t), false)) getOrElse
        ForcedConversion[D](null, true)
}

trait ForcedConversion_1 extends ForcedConversion_2 {
  implicit def forceConversion[T, D](t: T)(implicit ser: Serializer[T, D]) =
    ForcedConversion[D](ser.serialize(t), false)
}

trait ForcedConversion_2 {
  // The name of this method is significant for some additional checking done in the macro `contextMacro`.
  implicit def forceStringConversion[D, T: StringSerializer](value: T)(implicit ser: Serializer[String, D]) =
    ForcedConversion[D](ser.serialize(?[StringSerializer[T]].serialize(value)), false)
}

case class ForcedConversion[-D](value: Any, nothing: Boolean)

case class ParseException(source: String, line: Option[Int] = None, column: Option[Int] = None)
    extends Exception("Failed to parse source")
