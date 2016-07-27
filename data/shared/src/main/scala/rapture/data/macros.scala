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

package rapture.data

import rapture.base._
import rapture.core._

object Macros {

  // FIXME: Include enclosing position in the HashSet too
  val emittedWarnings = new collection.mutable.HashSet[String]

  def extractorMacro[T: c.WeakTypeTag, Data: c.WeakTypeTag, Th](
      c: WhiteboxContext): c.Expr[Extractor[T, Data]] = {
    import c.universe._
    import compatibility._

    val extractorType = typeOf[Extractor[_, _]].typeSymbol.asType.toTypeConstructor
    val nameMapperType = typeOf[NameMapper[_, _]].typeSymbol.asType.toTypeConstructor

    val implicitSearchFailures = collection.mutable.ListMap[String, List[String]]().withDefault(_ => Nil)

    val extractionType = weakTypeOf[T].typeSymbol.asClass

    if (weakTypeOf[T] <:< typeOf[AnyVal]) {
      val param = paramLists(c)(declarations(c)(weakTypeOf[T]).collect {
        case t: MethodSymbol => t.asMethod
      }.find(_.isPrimaryConstructor).get).head.head

      val paramType = param.typeSignature

      val inferredExtractor =
        c.inferImplicitValue(appliedType(extractorType, List(paramType, weakTypeOf[Data])), false, false)

      c.Expr[Extractor[T, Data]](q"$inferredExtractor.map { new ${weakTypeOf[T]}(_) }")

    } else if (extractionType.isSealed) {
      val subclasses = extractionType.knownDirectSubclasses.to[List]
      def tsort(todo: Map[Set[String], Symbol], done: List[Symbol] = Nil): List[Symbol] = if(todo.isEmpty) done else {
        val move = todo.filter { case (key, v) => (todo - key).forall(!_._1.subsetOf(key)) }
        tsort(todo -- move.map(_._1), move.map(_._2).to[List] ::: done)
      }

      val paramSets = subclasses.map { subclass =>
        (declarations(c)(subclass.asType.toType).collect {
          case m: MethodSymbol if m.isCaseAccessor => m.asMethod
        }.map(_.name.decodedName.toString).to[Set], subclass)
      }

      val sortedExtractors = tsort(paramSets.toMap).map { subclass =>
        val searchType = appliedType(extractorType, List(subclass.asType.toType, weakTypeOf[Data]))
        c.inferImplicitValue(searchType, false, false)
      }
      
      val NothingType = weakTypeOf[Nothing]
          
      val throwsType = sortedExtractors.last.tpe.member(typeName(c, "Throws")).typeSignature
      
      val combinedExtractors = sortedExtractors.reduceLeft { (left, right) => q"$left.orElse($right)" }
      
      c.Expr[Extractor[T, Data]](q"""
        (new _root_.rapture.data.Extractor[${weakTypeOf[T]}, ${weakTypeOf[Data]}] {
          def extract(data: ${weakTypeOf[Data]}, ast: _root_.rapture.data.DataAst, mode: _root_.rapture.core.Mode[_  <: _root_.rapture.core.MethodConstraint]): mode.Wrap[${weakTypeOf[
          T]}, Throws] = mode.wrap { $combinedExtractors }
        }).asInstanceOf[_root_.rapture.data.Extractor[${weakTypeOf[T]}, ${weakTypeOf[Data]}] {
          type Throws = ${throwsType}
        }]
      """)

    } else {

      require(weakTypeOf[T].typeSymbol.asClass.isCaseClass)

      val typeDeclaration = companion(c)(weakTypeOf[T].typeSymbol).typeSignature
      val valueParameters = paramLists(c)(declaration(c)(typeDeclaration, termName(c, "apply")).asMethod)
      val defaults = valueParameters.flatten.map(_.asTerm.isParamWithDefault).zipWithIndex.filter(_._1).map(_._2 + 1).to[Set]
      
      // FIXME integrate these into a fold
      var throwsTypes: Set[Type] = Set(typeOf[DataGetException])

      val params = declarations(c)(weakTypeOf[T]).collect {
        case m: MethodSymbol if m.isCaseAccessor => m.asMethod
      }.zipWithIndex map {
        case (p, idx) =>
          val nameMappingImplicit = c.inferImplicitValue(appliedType(nameMapperType, List(weakTypeOf[T], weakTypeOf[Data])), false, false)
          val deref = q"""data.selectDynamic($nameMappingImplicit.encode(${Literal(Constant(p.name.decodedName.toString))}))"""

          val NothingType = weakTypeOf[Nothing]

          val inferredImplicit =
            try c.inferImplicitValue(appliedType(extractorType, List(p.returnType, weakTypeOf[Data])), false, false)
            catch {
              case e: Exception =>
                implicitSearchFailures(p.returnType.toString) ::= p.name.decodedName.toString
                null
            }

          val t = try {
            inferredImplicit.tpe.member(typeName(c, "Throws")).typeSignature match {
              case NothingType => List()
              case refinedType: RefinedType => refinedType.parents
              case typ: Type => List(typ)
              case _ => ???
            }
          } catch {
            case e: Exception => List()
          }

          if(!defaults.contains(idx + 1)) throwsTypes ++= t

          // Borrowed from Shapeless
          def companionRef(tpe: Type): Tree = {
            val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
            val gTpe = tpe.asInstanceOf[global.Type]
            val pre = gTpe.prefix
            val sym = gTpe.typeSymbol
            val cSym = sym.companionSymbol
            if (cSym != NoSymbol) global.gen.mkAttributedRef(pre, cSym).asInstanceOf[Tree]
            else Ident(tpe.typeSymbol.name.toTermName)
          }


          if (defaults.contains(idx + 1)) q"""
          mode.unwrap(try $deref.as($inferredImplicit, mode.generic) catch { case e: Exception => mode.wrap(${companionRef(weakTypeOf[T])}.${termName(
              c, "apply$default$" + (idx + 1))}.asInstanceOf[${p.returnType}]) }, ${Literal(Constant("." + p.name.decodedName))})
          """ else q"""
          mode.unwrap($deref.as($inferredImplicit, mode.generic), ${Literal(Constant("." + p.name.decodedName))})
        """
      }

      if (implicitSearchFailures.nonEmpty) {
        val paramStrings = implicitSearchFailures map {
          case (t, p1 :: Nil) =>
            s"parameter `$p1' of type $t"
          case (t, p1 :: ps) =>
            s"parameters ${ps.map(v => s"`$v'").mkString(", ")} and `$p1' of type $t"
          case (t, Nil) =>
            ??? // Doesn't happen
        }
        val errorString = paramStrings.to[List].reverse match {
          case t1 :: Nil => t1
          case t1 :: ts => s"${ts.mkString(", ")} and $t1"
          case Nil => ??? // Doesn't happen
        }
        val plural =
          if (implicitSearchFailures.flatMap(_._2).size > 1)
            s"${weakTypeOf[Data].typeSymbol.name} extractors"
          else
            s"a ${weakTypeOf[Data].typeSymbol.name} extractor"

        val err = s"Could not generate a ${weakTypeOf[Data].typeSymbol.name} extractor for case " +
            s"class ${weakTypeOf[T].typeSymbol.name} because $plural for $errorString could not " +
            s"be found"

        if (!emittedWarnings.contains(err)) {
          emittedWarnings += err
          c.warning(NoPosition, err)
        }
      }

      require(implicitSearchFailures.isEmpty)

      val construction = c.Expr[T](q"""new ${weakTypeOf[T]}(..$params)""")

      c.Expr[Extractor[T, Data]](q"""
        (new _root_.rapture.data.Extractor[${weakTypeOf[T]}, ${weakTypeOf[Data]}] {
          def extract(data: ${weakTypeOf[Data]}, ast: _root_.rapture.data.DataAst, mode: _root_.rapture.core.Mode[_  <: _root_.rapture.core.MethodConstraint]): mode.Wrap[${weakTypeOf[
          T]}, Throws] = mode.wrap { ${construction} }
        }).asInstanceOf[_root_.rapture.data.Extractor[${weakTypeOf[T]}, ${weakTypeOf[Data]}] {
          type Throws = ${normalize(c)(typeIntersection(c)(throwsTypes.to[List]))}
        }]
      """)
    }
  }

  def serializerMacro[T: c.WeakTypeTag, Data: c.WeakTypeTag](c: WhiteboxContext)(
      ast: c.Expr[DataAst]): c.Expr[Serializer[T, Data]] = {
    import c.universe._
    import compatibility._

    val tpe = weakTypeOf[T].typeSymbol.asClass
    val nameMapperType = typeOf[NameMapper[_, _]].typeSymbol.asType.toTypeConstructor
    val serializer = typeOf[Serializer[_, _]].typeSymbol.asType.toTypeConstructor

    if (weakTypeOf[T] <:< typeOf[AnyVal]) {

      val param = paramLists(c)(declarations(c)(weakTypeOf[T]).collect {
        case t: MethodSymbol => t.asMethod
      }.find(_.isPrimaryConstructor).get).head.head

      val paramName = param.name.decodedName.toString
      val paramType = param.typeSignature
      val inferredSerializer =
        c.inferImplicitValue(appliedType(serializer, List(paramType, weakTypeOf[Data])), false, false)

      val newName = termName(c, freshName(c)("param$"))

      c.Expr[Serializer[T, Data]](q"$inferredSerializer.contramap[${weakTypeOf[T]}](_.${termName(c, paramName)})")
    } else {
      if (tpe.isCaseClass) {
        val params = declarations(c)(weakTypeOf[T]).collect {
          case m: MethodSymbol if m.isCaseAccessor => m.asMethod
        }.map { p =>
          val imp = c.inferImplicitValue(appliedType(serializer, List(p.returnType, weakTypeOf[Data])), false, false)
          val appliedSerializerType = appliedType(nameMapperType, List(weakTypeOf[T], weakTypeOf[Data]))
          val nameMapperImplicit = c.inferImplicitValue(appliedSerializerType, false, false)
          q"""($nameMapperImplicit.encode(${Literal(Constant(p.name.decodedName.toString))}),
              $imp.serialize(${termName(c, "t")}.${p}))"""
        }

        c.Expr[Serializer[T, Data]](q"""new _root_.rapture.data.Serializer[${weakTypeOf[T]}, ${weakTypeOf[Data]}] {
          def serialize(t: ${weakTypeOf[T]}): _root_.scala.Any =
            $ast.fromObject(_root_.scala.collection.immutable.Map(..$params).filterNot { v => $ast.isNull(v._2) })
        }""")
      } else if (tpe.isSealed) {
        val cases = tpe.knownDirectSubclasses.to[List]
        val caseClauses = cases.map { sc =>
          val fullySpecifiedSerializer = appliedType(serializer, List(sc.asType.toType, weakTypeOf[Data]))
          val caseSerializer = c.inferImplicitValue(fullySpecifiedSerializer, false, false)
          val pattern = pq"v: ${sc.asClass}"
          cq"${pattern} => $caseSerializer.serialize(v)"
        }
        
        c.Expr[Serializer[T, Data]](q"""new _root_.rapture.data.Serializer[${weakTypeOf[T]}, ${weakTypeOf[Data]}] {
          def serialize(t: ${weakTypeOf[T]}): _root_.scala.Any = t match { case ..$caseClauses }
        }""")
      } else throw new Exception()

    }
  }
}
