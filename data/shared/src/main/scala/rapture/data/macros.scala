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

import rapture.base._
import rapture.core._

object Macros {

  // FIXME: Include enclosing position in the HashSet too
  val emittedWarnings = new collection.mutable.HashSet[String]

  // FIXME: Consider adding this check for knownDirectSubclasses to support sealed case classes
  // def impl[T: c.WeakTypeTag](c: WhiteboxContext): c.Tree = {
  //   import c.universe._
  //   def subs = weakTypeOf[T].typeSymbol.asClass.knownDirectSubclasses
  //   val subs1 = subs
  //   println(subs1)
  //   val global = c.universe.asInstanceOf[tools.nsc.Global]
  //   def checkSubsPostTyper = if (subs1 != subs)
  //     c.error(c.macroApplication.pos, "sealed descendents appears after macro exansion")
  //     val dummyTypTree =
  //     new global.TypeTreeWithDeferredRefCheck()(() => { checkSubsPostTyper ; global.TypeTree(global.NoType) }).asInstanceOf[TypTree]
  //   q"type T = $dummyTypTree; ${subs1.size} : _root_.scala.Int"
  // }
  // def numChildren[T]: Int = macro impl[T]


  def extractorMacro[T: c.WeakTypeTag, Data: c.WeakTypeTag, Th](c: WhiteboxContext): c.Expr[Extractor[T, Data] { type Throws = Th }] = {
    import c.universe._
    import compatibility._

    val extractor = typeOf[Extractor[_, _]].typeSymbol.asType.toTypeConstructor

    val implicitSearchFailures =
      collection.mutable.ListMap[String, List[String]]().withDefault(_ => Nil)

    if(weakTypeOf[T] <:< typeOf[AnyVal]) {
      val param = paramLists(c)(declarations(c)(weakTypeOf[T]).collect {
        case t: MethodSymbol => t.asMethod
      }.find(_.isPrimaryConstructor).get).head.head
      
      val paramType = param.typeSignature
      
      val inferredExtractor = c.inferImplicitValue(appliedType(extractor, List(paramType,
          weakTypeOf[Data])), false, false)
      
      val newName = termName(c, freshName(c)("param$"))
      
      c.Expr(Apply(
        Select(
          inferredExtractor, termName(c, "map")
        ),
        List(
          Function(
            List(
              ValDef(
                Modifiers(Flag.PARAM),
                newName,
                TypeTree(),
                EmptyTree
              )
            ),
            Apply(
              Select(
                New(TypeTree(weakTypeOf[T])),
                constructor(c)
              ),
              List(
                Ident(newName)
              )
            )
          )
        )
      ))
    } else {
      require(weakTypeOf[T].typeSymbol.asClass.isCaseClass)

      val defaults = weakTypeOf[T].typeSymbol.companionSymbol.typeSignature.declarations.to[List].map(_.name.decodedName.toString).filter(_ startsWith "apply$default$").map(_.substring(14).toInt).to[Set]

      // FIXME integrate these into a fold
      var throwsTypes: Set[Type] = Set(typeOf[DataGetException])

      val params = declarations(c)(weakTypeOf[T]).collect {
        case m: MethodSymbol if m.isCaseAccessor => m.asMethod
      }.zipWithIndex map { case (p, idx) =>
        val deref = q"""data.selectDynamic(${Literal(Constant(p.name.decodedName.toString))})"""

        val NothingType = weakTypeOf[Nothing]
        
        val imp = try c.inferImplicitValue(appliedType(extractor, List(p.returnType, weakTypeOf[Data])), false,
            false) catch {
          
          case e: Exception =>
            implicitSearchFailures(p.returnType.toString) ::= p.name.decodedName.toString
            null
        }

        val t = try {
          imp.tpe.member(typeName(c, "Throws")).typeSignature match {
            case NothingType => List()
            case refinedType: RefinedType => refinedType.parents
            case typ: Type => List(typ)
            case _ => ???
          }
        } catch {
          case e: Exception =>
            List()
        }

        throwsTypes ++= t

        // Borrowed from Shapeless
        def companionRef(tpe: Type): Tree = {
          val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
          val gTpe = tpe.asInstanceOf[global.Type]
          val pre = gTpe.prefix
          val sym = gTpe.typeSymbol
          val cSym = sym.companionSymbol
          if(cSym != NoSymbol) global.gen.mkAttributedRef(pre, cSym).asInstanceOf[Tree]
          else Ident(tpe.typeSymbol.name.toTermName)
        }  

        if(defaults.contains(idx + 1)) q"""
          mode.unwrap(if($deref.is($imp)) $deref.as($imp, mode.generic) else mode.wrap(${companionRef(weakTypeOf[T])}.${termName(c, "apply$default$"+(idx + 1))}.asInstanceOf[${p.returnType}]), ${Literal(Constant("."+p.name.decodedName))})
        """ else q"""
          mode.unwrap($deref.as($imp, mode.generic), ${Literal(Constant("."+p.name.decodedName))})
        """
      }

      if(!implicitSearchFailures.isEmpty) {
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
        val plural = if(implicitSearchFailures.flatMap(_._2).size > 1)
            s"${weakTypeOf[Data].typeSymbol.name} extractors" else
            s"a ${weakTypeOf[Data].typeSymbol.name} extractor"
        
        val err = s"Could not generate a ${weakTypeOf[Data].typeSymbol.name} extractor for case "+
            s"class ${weakTypeOf[T].typeSymbol.name} because $plural for $errorString could not "+
            s"be found"
        
        if(!emittedWarnings.contains(err)) {
          emittedWarnings += err
          c.warning(NoPosition, err)
        }
      }

      require(implicitSearchFailures.isEmpty)
      
      val construction = c.Expr[T](
        Apply(
          Select(
            New(
              TypeTree(weakTypeOf[T])
            ),
            constructor(c)
          ),
          params.to[List]
        )
      )

      c.Expr(q"""
        (new _root_.rapture.data.Extractor[${weakTypeOf[T]}, ${weakTypeOf[Data]}] {
          def extract(data: ${weakTypeOf[Data]}, ast: _root_.rapture.data.DataAst, mode: _root_.rapture.core.Mode[_  <: _root_.rapture.core.MethodConstraint]): mode.Wrap[${weakTypeOf[T]}, Throws] = mode.wrap { ${construction} }
        }).asInstanceOf[_root_.rapture.data.Extractor[${weakTypeOf[T]}, ${weakTypeOf[Data]}] {
          type Throws = ${normalize(c)(typeIntersection(c)(throwsTypes.to[List]))}
        }]
      """)
    }
  }

  def serializerMacro[T: c.WeakTypeTag, Data: c.WeakTypeTag](c: WhiteboxContext)(ast: c.Expr[DataAst]):
      c.Expr[Serializer[T, Data]] = {
    import c.universe._
    import compatibility._

    val tpe = weakTypeOf[T].typeSymbol.asClass
    val serializer = typeOf[Serializer[_, _]].typeSymbol.asType.toTypeConstructor

    if(weakTypeOf[T] <:< typeOf[AnyVal]) {
      
      val param = paramLists(c)(declarations(c)(weakTypeOf[T]).collect {
        case t: MethodSymbol => t.asMethod
      }.find(_.isPrimaryConstructor).get).head.head
      
      val paramName = param.name.decodedName.toString
      val paramType = param.typeSignature
      val inferredSerializer = c.inferImplicitValue(appliedType(serializer, List(paramType,
          weakTypeOf[Data])), false, false)

      val newName = termName(c, freshName(c)("param$"))
      
      c.Expr(Apply(
        Select(
          inferredSerializer, termName(c, "contramap")
        ),
        List(
          Function(
            List(
              ValDef(
                Modifiers(Flag.PARAM),
                newName,
                TypeTree(weakTypeOf[T]),
                EmptyTree
              )
            ),
            Select(
              Ident(newName),
              termName(c, paramName)
            )
          )
        )
      ))
    } else {
      val construction = if(tpe.isCaseClass) {

        val params = declarations(c)(weakTypeOf[T]) collect {
          case m: MethodSymbol if m.isCaseAccessor => m.asMethod
        } map { p =>
          Apply(
            Select(
              Ident("scala"),
              termName(c, "Tuple2")
            ),
            List(
              Literal(Constant(p.name.decodedName.toString)),
              Apply(
                Select(
                  c.inferImplicitValue(appliedType(serializer, List(p.returnType,
                      weakTypeOf[Data])), false, false),
                  termName(c, "serialize")
                ),
                List(
                  Select(
                    Ident(termName(c, "t")),
                    p.name
                  )
                )
              )
            )
          )
        }

        c.Expr[Map[String, Any]](
          Apply(
            Select(
              Select(
                Ident(definitions.PredefModule),
                termName(c, "Map")
              ),
              termName(c, "apply")
            ),
            params.to[List]
          )
        )
      } else if(tpe.isSealed) {
        c.Expr[Map[String, Any]](
          Match(
            Ident(termName(c, "t")),
            tpe.knownDirectSubclasses.to[List] map { sc =>
              CaseDef(
                Bind(
                  termName(c, "v"),
                  Typed(
                    Ident(wildcard(c)),
                    Ident(sc.asClass)
                  )
                ),
                EmptyTree,
                Apply(
                  Select(
                    c.inferImplicitValue(appliedType(serializer, List(sc.asType.toType)), false,
                        false),
                    termName(c, "serialize")
                  ),
                  List(Ident(termName(c, "v")))
                )
              )
            }
          )
        )
      } else throw new Exception()

      reify(new Serializer[T, Data] {
        def serialize(t: T): Any = ast.splice.fromObject(construction.splice.filterNot { v =>
          ast.splice.isNull(v._2)
        })
      })
    }
  }
}
