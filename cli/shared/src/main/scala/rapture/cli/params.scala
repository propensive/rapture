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

package rapture.cli

import rapture.fs._
import rapture.core._

import annotation.tailrec

import scala.collection.immutable.ListMap

import language.higherKinds

object Optable {
  implicit val stringOptable = new Optable[String] {
    def name(t: String) = t
    def description(t: String) = Vector()
    def hidden(t: String): Boolean = false
  }

  implicit val OptOptable = new Optable[Opt] {
    def name(t: Opt) = t.name
    def description(t: Opt) = Vector(s"-- ${t.description}")
    def hidden(t: Opt): Boolean = t.hidden
  }

  implicit val stringPairOptable = new Optable[(String, String)] {
    def name(t: (String, String)) = t._1
    def description(t: (String, String)) = Vector(t._2)
    def hidden(t: (String, String)): Boolean = t._2 == ""
  }
}

trait Optable[-T] {
  def name(t: T): String
  def description(t: T): Vector[String]
  def hidden(t: T): Boolean
}

case class Opt(
    val name: String, val description: String, val hidden: Boolean = false)(
    opts: => Opts[Opt]) {
  def unapply(arg: Arg): Boolean = opts.unapply(arg) == Some(this)
}

case class Opts[T : Optable](options: T*) {

  private val optable = implicitly[Optable[T]]

  def unapply(arg: Arg): Option[T] = {
    val p = arg(suggester)
    options.to[List].find(optable.name(_) == p)
  }

  def suggester: Suggester = new Suggester {
    override def suggest(prefix: String): Suggestions =
      Suggestions(
          SuggestionGroup(None,
                          options
                            .to[Vector]
                            .map { opt =>
                          (optable.name(opt), optable.description(opt))
                        }
                            .filter(_._1 startsWith prefix)
                            .toMap,
                          false))
  }
}

object NoSuggestions extends Suggester

case class Param[+T : ParamParser](longName: String = null,
                                   shortName: String = null,
                                   description: String = null,
                                   suggester: Suggester = NoSuggestions,
                                   repeatable: Boolean = false) {
  def parse(s: List[String]): Option[T] = implicitly[ParamParser[T]].parse(s)
}

trait ParamParser_1 {
  implicit val intParamParser: ParamParser[Int] = new ParamParser[Int] {
    def parse(s: List[String]) = s.headOption.map(_.toInt)
  }

  implicit val booleanParamParser: ParamParser[Boolean] =
    new ParamParser[Boolean] {
      def parse(s: List[String]): Option[Boolean] = Some(true)
    }
}

object ParamParser extends ParamParser_1 {
  implicit val defaultParamParser: ParamParser[String] =
    new ParamParser[String] { def parse(s: List[String]) = s.headOption }

  def of[T : ParamParser]: ParamParser[T] = implicitly[ParamParser[T]]
}

trait ParamParser[+T] { paramParser =>
  def parse(s: List[String]): Option[T]
  def map[S](fn: T => S): ParamParser[S] = new ParamParser[S] {
    def parse(s: List[String]): Option[S] = paramParser.parse(s).map(fn)
  }
}

object Paramable {
  implicit def paramParamable[T]: Paramable[T, Param] =
    new Paramable[T, Param] {
      def longName(p: Param[T]): String =
        Option(p.longName).map("--" + _).getOrElse("")
      def shortName(p: Param[T]): String =
        Option(p.shortName).map("-" + _).getOrElse("")
      def description(p: Param[T]): Vector[String] =
        Option(p.description).to[Vector]
      def hidden(p: Param[T]): Boolean = false
      def suggester(p: Param[T]): Suggester = p.suggester
      def repeatable(p: Param[T]): Boolean = p.repeatable
      def parse(p: Param[T], s: List[String]): Option[T] = p.parse(s)
    }
}

trait Paramable[T, P[_]] {
  def longName(t: P[T]): String
  def shortName(t: P[T]): String
  def description(t: P[T]): Vector[String]
  def hidden(t: P[T]): Boolean
  def suggester(t: P[T]): Suggester
  def repeatable(t: P[T]): Boolean
  def parse(p: P[T], s: List[String]): Option[T]
}

case class Params[
    T, P[_]](options: P[T]*)(implicit paramable: Paramable[T, P]) {

  def suggester(exclusions: Set[String]): Suggester = new Suggester {
    override def suggest(prefix: String): Suggestions = Suggestions(
        SuggestionGroup(None, options.filterNot { opt =>
          exclusions.contains(paramable.longName(opt).drop(2)) ||
          exclusions.contains(paramable.shortName(opt).drop(1))
        }.map { opt =>
          (paramable.longName(opt),
           paramable.shortName(opt) +: paramable.description(opt))
        }.filter(_._1 startsWith prefix).toMap, false),
        SuggestionGroup(None, options.filterNot { opt =>
          exclusions.contains(paramable.longName(opt).drop(2)) ||
          exclusions.contains(paramable.shortName(opt).drop(1))
        }.map { opt =>
          paramable.shortName(opt) -> Vector()
        }.filter(_._1 startsWith prefix).toMap, true)
    )
  }

  private object LongOpt {
    def unapply(arg: Arg): Option[String] =
      if (arg.param.startsWith("--")) Some(arg(suggester(Set())).drop(2))
      else None
  }

  private object ShortOpt {
    def unapply(arg: Arg): Option[String] =
      if (arg.param.startsWith("-"))
        Some(arg(suggester(Set())).drop(1).take(1)) else None
  }

  @tailrec
  private def organize(
      xs: Seq[Arg],
      acc: ListMap[String, List[Arg]] = ListMap()): Map[String, List[Arg]] =
    xs match {
      case Seq() =>
        acc
      case LongOpt(dashParam) +: tail =>
        organize(tail, acc + (dashParam -> Nil))
      case (arg @ ShortOpt(dashParam)) +: tail =>
        if (arg.param.length == 2) organize(tail, acc + (dashParam -> Nil))
        else
          organize(arg.copy(param = "-" + arg.param.drop(2)) +: tail,
                   acc + (dashParam.take(2) -> Nil))
      case v +: tail =>
        acc.lastOption match {
          case Some((key, entry)) =>
            organize(tail, acc + (key -> (entry :+ v)))
          case None =>
            val exclusions = acc.keys.to[Set]
            v(suggester(exclusions))
            organize(tail, acc)
        }
    }

  def unapply(cmdLine: CmdLine): Option[ParamMap] =
    Some(ParamMap(organize(cmdLine.params), cmdLine.completer))
}

case class ParamMap(
    params: Map[String, List[Arg]], completer: Option[Completer]) {

  def get[T, P[_]](param: P[T], suggesters: Suggester*)(
      implicit paramable: Paramable[T, P]): Option[T] = {
    params.find {
      case (k, v) =>
        k == implicitly[Paramable[T, P]].longName(param).substring(2) ||
        k == implicitly[Paramable[T, P]].shortName(param).substring(1)
    }.flatMap {
      case (_, args) =>
        val values =
          suggesters padTo (args.length, NoSuggestions) zip args map {
            case (suggester, arg) => arg(suggester)
          }
        paramable.parse(param, values.to[List])
    }
  }

  def apply[T, P[_]](param: P[T], suggesters: Suggester*)(
      implicit paramable: Paramable[T, P]): T =
    get(param, suggesters: _*).getOrElse {
      if (completer.isDefined) null.asInstanceOf[T]
      else throw ParamGetException(paramable.longName(param))
    }
}

object -- {
  def unapply(cmdLine: CmdLine): Option[(CmdLine, CmdLine)] = {
    val left = cmdLine.params.takeWhile(_.param != "--")
    Some(
        if (cmdLine.params.length == left.length)
          (cmdLine, CmdLine(cmdLine.pwd, Vector(), cmdLine.completer))
        else
          (
              CmdLine(cmdLine.pwd, left, cmdLine.completer),
              CmdLine(cmdLine.pwd,
                      cmdLine.params.drop(left.length + 1),
                      cmdLine.completer)
          )
    )
  }
}

case class Arg(param: String, completer: Option[Completer], current: Boolean) {
  def apply(suggester: Suggester) = completer match {
    case Some(c) if current =>
      c.process(suggester.suggest(c.prefix))
    case _ => param
  }
}

case class CmdLine(
    pwd: FsUrl, params: Vector[Arg], completer: Option[Completer])
    extends collection.SeqLike[Arg, CmdLine] {

  def seq = params
  def apply(idx: Int) = params(idx)
  def length = params.length
  def iterator: Iterator[Arg] = params.iterator
  def newBuilder: collection.mutable.Builder[Arg, CmdLine] =
    new collection.mutable.Builder[Arg, CmdLine] {
      var vect: Vector[Arg] = Vector()
      def clear(): Unit = vect = Vector()
      def result(): CmdLine = CmdLine(pwd, vect, completer)
      def +=(elem: Arg) = {
        vect = vect :+ elem
        this
      }
    }

  override def toString =
    params.map {
      case Arg(p, _, false) => p
      case Arg(p, _, true) =>
        completer.get.prefix + "^" + p.drop(completer.get.prefix.length + 1)
    }.mkString(" ")
}

class Suggester { def suggest(prefix: String): Suggestions = Suggestions() }
object Suggestions {
  def from[T](it: Iterable[T])(fn: T => String, fns: (T => Any)*) =
    new Suggester {
      override def suggest(prefix: String) =
        Suggestions(
            SuggestionGroup(None, it.map { e =>
          fn(e) ->
          ((if (fns.isEmpty)
              "" else "--") +: fns.to[Vector].map(_ (e).toString))
        }.filter(_._1.startsWith(prefix)).toMap, false))
    }

  /*def from[T](fromPrefix: String => Iterable[T])(fn: T => String, fns: (T => Any)*) = new Suggester {
    override def suggest(prefix: String) = Suggestions(SuggestionGroup(None,
      fromPrefix(prefix).map { e =>
        fn(e) -> ((if(fns.isEmpty) "" else "--") +: fns.to[Vector].map(_(e).toString))
      }.toMap, false))
  }*/
}
case class Suggestions(groups: SuggestionGroup*)
case class SuggestionGroup(title: Option[String],
                           suggestions: Map[String, Vector[String]],
                           hidden: Boolean)

case class Completer(prefix: String, shellCompleter: Suggestions => Nothing) {
  def process(suggestions: Suggestions): Nothing = shellCompleter(suggestions)
}

trait Completions[ShellTypes <: Shell] {
  this: CliApp =>

  override def makeCmdLine(pwd: FsUrl, args: Vector[String]): CmdLine =
    shellCompleter().makeCmdLine(pwd, args)

  def shellCompleter(): ShellTypes
}
