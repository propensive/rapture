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

package rapture.io
import rapture.core._
import rapture.codec._
import rapture.mime._

import java.io._
import scala.reflect._

import language.higherKinds

trait Closable[-R] { def close(resource: R): Unit }

object Utils {

  /** Safely closes a stream after processing */
  def ensuring[Res, Strm: Closable](create: Strm)(blk: Strm => Res) = {
    val stream = create
    val result = try { blk(stream) } catch {
      case e: Throwable =>
        try { implicitly[Closable[Strm]].close(stream) } catch { case e2: Exception => () }
        throw e
    }

    implicitly[Closable[Strm]].close(stream)

    result
  }
}

case class StringIsInput(string: String) extends CharInput(alloc[StringReader](string))

case class ByteArrayInput(array: Array[Byte]) extends ByteInput(alloc[ByteArrayInputStream](array))

object InputBuilder {
  implicit def stringInputBuilder(implicit encoding: Encoding): InputBuilder[InputStream, String] =
    new InputBuilder[InputStream, String] {
      def input(s: InputStream): Input[String] =
        alloc[LineInput](alloc[InputStreamReader](s, encoding.name))
    }

  implicit def inputStreamCharBuilder(implicit encoding: Encoding): InputBuilder[InputStream, Char] =
    new InputBuilder[InputStream, Char] {
      def input(s: InputStream): Input[Char] =
        alloc[CharInput](alloc[InputStreamReader](s, encoding.name))
    }
  implicit val buildInputStream: InputBuilder[InputStream, Byte] = InputStreamBuilder
  implicit val buildReader: InputBuilder[java.io.Reader, Char] = ReaderBuilder
  implicit val buildLineReader: InputBuilder[java.io.Reader, String] = LineReaderBuilder
}

object Input {
  implicit def inputClosable[T]: Closable[Input[T]] = new Closable[Input[T]] {
    def close(in: Input[T]): Unit = in.close()
  }
}

trait InputBuilder[InputType, Data] {
  def input(s: InputType): Input[Data]
}

object OutputBuilder {
  implicit val buildOutputStream: OutputBuilder[OutputStream, Byte] = OutputStreamBuilder
  implicit val buildWriter: OutputBuilder[java.io.Writer, Char] = WriterBuilder

  implicit def stringOutputBuilder(implicit encoding: Encoding): OutputBuilder[OutputStream, String] =
    new OutputBuilder[OutputStream, String] {
      def output(s: OutputStream): Output[String] =
        new LineOutput(new OutputStreamWriter(s, encoding.name))
    }

  implicit def outputStreamCharBuilder(implicit encoding: Encoding): OutputBuilder[OutputStream, Char] =
    new OutputBuilder[OutputStream, Char] {
      def output(s: OutputStream): Output[Char] =
        alloc[CharOutput](alloc[OutputStreamWriter](s, encoding.name))
    }

}

trait OutputBuilder[OutputType, Data] {
  def output(s: OutputType): Output[Data]
}

object AppenderBuilder {
  implicit def buildAppender: AppenderBuilder[java.io.Writer, Char] =
    new AppenderBuilder[java.io.Writer, Char] {
      def appendOutput(s: java.io.Writer) = alloc[CharOutput](s)
    }
}

trait AppenderBuilder[OutputType, Data] { def appendOutput(s: OutputType): Output[Data] }

object Appendable {
  class Capability[Res](res: Res) {
    def appendOutput[Data](implicit sa: Appender[Res, Data], mode: Mode[`Appendable#appendOutput`]) =
      sa.appendOutput(res)

    def handleAppend[Data, Res2](body: Output[Data] => Res2)(implicit sw: Appender[Res, Data]): Res2 = {
      ensuring(appendOutput[Data])(body)
    }
  }

}

object Readable {
  class Capability[Res](res: Res) {

    def input[Data](implicit sr: Reader[Res, Data], mode: Mode[`Readable#input`]): mode.Wrap[Input[Data], Exception] =
      mode.wrap(sr.input(res))

    def redirectTo[Data, DestRes](dest: DestRes)(implicit sr: Reader[Res, Data],
                                                 sw: Writer[DestRes, Data],
                                                 mode: Mode[`Readable#redirectTo`],
                                                 mf: ClassTag[Data]): mode.Wrap[Int, Exception] =
      mode.wrap(handleInput[Data, Int] { in =>
        writable(dest).handleOutput[Data, Int](in.pumpTo)
      })

    def >[Data, DestRes](dest: DestRes)(implicit sr: Reader[Res, Data],
                                        sw: Writer[DestRes, Data],
                                        mode: Mode[`Readable#redirectTo`],
                                        mf: ClassTag[Data]): mode.Wrap[Int, Exception] =
      redirectTo[Data, DestRes](dest)(sr, sw, mode, mf)

    def pipeTo[Data, DestRes](
        dest: DestRes)(implicit sr: Reader[Res, Data], sw: Writer[DestRes, Data], mf: ClassTag[Data]): DestRes = {
      redirectTo(dest)
      dest
    }

    def |[Data, DestRes](
        dest: DestRes)(implicit sr: Reader[Res, Data], sw: Writer[DestRes, Data], mf: ClassTag[Data]): DestRes =
      pipeTo(dest)(sr, sw, mf)

    def >>[Data, DestRes](dest: DestRes)(implicit sr: Reader[Res, Data],
                                         sw: Appender[DestRes, Data],
                                         mode: Mode[`Readable#appendTo`],
                                         mf: ClassTag[Data]): mode.Wrap[Int, Exception] =
      mode.wrap(handleInput[Data, Int] { in =>
        dest.handleAppend[Data, Int](in.pumpTo)
      })

    def >[Data](out: Output[Data])(implicit sr: Reader[Res, Data],
                                   mode: Mode[`Readable#redirectTo`],
                                   mf: ClassTag[Data]): mode.Wrap[Int, Exception] =
      mode.wrap(handleInput[Data, Int](_ pumpTo out))

    def handleInput[Data, Res2](body: Input[Data] => Res2)(implicit sr: Reader[Res, Data]): Res2 =
      ensuring(input[Data])(body)
  }
}

object Writable {
  class Capability[Res](res: Res) {

    def output[Data](implicit sw: Writer[Res, Data],
                     mode: Mode[`Writable#output`]): mode.Wrap[Output[Data], Exception] = mode.wrap(sw.output(res))

    def handleOutput[Data, Res2](body: Output[Data] => Res2)(implicit sw: Writer[Res, Data]): Res2 =
      ensuring(output[Data])(body)
  }
}

object Writer {
  implicit def byteToLineWriters[T](implicit jisw: JavaOutputStreamWriter[T], encoding: Encoding): Writer[T, String] =
    new Writer[T, String] {
      def output(t: T): Output[String] = alloc[LineOutput](alloc[OutputStreamWriter](jisw.getOutputStream(t)))
    }

  implicit def byteToCharWriters[T](implicit jisw: JavaOutputStreamWriter[T], encoding: Encoding): Writer[T, Char] =
    new Writer[T, Char] {
      def output(t: T): Output[Char] = alloc[CharOutput](alloc[OutputStreamWriter](jisw.getOutputStream(t)))
    }

  implicit val stdoutWriter: JavaOutputStreamWriter[Stdout.type] =
    new JavaOutputStreamWriter[Stdout.type](x => System.out, _ => ())

  implicit val stderrWriter: Writer[Stderr.type, Byte] = new Writer[Stderr.type, Byte] {
    def output(stderr: Stderr.type): Output[Byte] =
      ?[OutputBuilder[OutputStream, Byte]].output(System.out)
  }
}

@implicitNotFound(
    msg = "Cannot write $"+"{Data} data to $"+"{Resource} resources. Note that if you " +
        "are working with Char data, you will require an implicit character encoding, e.g. " +
        "import encodings.system._ or import encodings.`UTF-8`._.")
trait Writer[-Resource, @specialized(Byte, Char) Data] {
  def output(res: Resource): Output[Data]
}

object Appender extends Appender_1 {

  implicit def byteToCharAppenders[T](implicit jisw: JavaOutputAppender[T], encoding: Encoding): Appender[T, Char] =
    new Appender[T, Char] {
      def appendOutput(t: T): Output[Char] =
        alloc[CharOutput](alloc[OutputStreamWriter](jisw.getOutputStream(t)))
    }

  implicit def byteToLineAppender[Res](implicit appender: Appender[Res, Byte], enc: Encoding) = {
    new Appender[Res, String] {
      def appendOutput(res: Res): Output[String] = new Output[String] {
        private lazy val output = appender.appendOutput(res)
        def close() = output.close()
        def flush() = output.flush()
        def write(s: String) = {
          output.writeBlock((s + "\n").getBytes(enc.name))
          output.flush()
        }
      }
    }
  }

  implicit val stdoutAppender: JavaOutputAppender[Stdout.type] =
    new JavaOutputAppender[Stdout.type](x => System.out, _ => ())

  implicit val stderrAppender: JavaOutputAppender[Stderr.type] =
    new JavaOutputAppender[Stderr.type](x => System.err, _ => ())

  implicit val stdoutCharAppender: Appender[Stdout.type, Char] =
    byteToCharAppenders(stdoutAppender, encodings.system())

  implicit val stderrCharAppender: Appender[Stderr.type, Char] =
    byteToCharAppenders(stderrAppender, encodings.system())

}

trait Appender_1 {
  implicit def charToLineAppender[Res](implicit appender: Appender[Res, Char]) = {
    new Appender[Res, String] {
      def appendOutput(res: Res): Output[String] = new Output[String] {
        private lazy val output = appender.appendOutput(res)
        def close() = output.close()
        def flush() = output.flush()
        def write(s: String) = {
          output.writeBlock((s + "\n").to[Array])
          output.flush()
        }
      }
    }
  }
}

trait Appender[-Resource, Data] {
  def appendOutput(res: Resource): Output[Data]
}

trait TypedInput { thisInput: Input[_] =>
  def mimeType: MimeTypes.MimeType
}

trait Input[@specialized(Byte, Char) Data] extends Seq[Data] { thisInput =>

  private var beingHandled = false

  override def toString() = "<input>"

  // FIXME: This just shouldn't be a Seq
  def length: Int = throw alloc[Exception]("Cannot calculate length of a stream")

  def apply(n: Int) = {
    for (i <- 0 until n) read()
    read().get
  }

  def typed(mime: MimeTypes.MimeType) = new Input[Data] with TypedInput {
    def mimeType = mime
    def ready() = thisInput.ready()
    def close() = thisInput.close()
    def read() = thisInput.read()
  }

  def iterator: Iterator[Data] = new Iterator[Data] {
    private var nextVal: Option[Data] = read()
    def hasNext = nextVal.isDefined
    def next() = {
      val x = nextVal.get
      nextVal = read()
      x
    }
  }

  def ready(): Boolean

  def read(): Option[Data]

  def readBlock(array: Array[Data], offset: Int = 0, length: Int = -1): Int = {

    val end = if (length < 0) array.length - offset else offset + length

    read() match {
      case None => -1
      case Some(c) =>
        array(offset) = c
        var i = offset + 1
        var continue = true
        while (i < end && continue) {
          read() match {
            case None =>
              continue = false
            case Some(c) =>
              array(i) = c
              i += 1
          }
        }

        i - offset
    }
  }

  def close(): Unit

  def pumpTo(out: Output[Data])(implicit mf: ClassTag[Data]): Int = {
    val buf: Array[Data] = alloc(65536)
    var len = readBlock(buf)
    var count = 0
    while (len >= 0) {
      out.writeBlock(buf, length = len)
      count += len
      len = readBlock(buf)
    }
    count
  }

  def map[T](fn: Data => T): Input[T] = new Input[T] {
    def read(): Option[T] = thisInput.read().map(fn)
    def ready(): Boolean = thisInput.ready()
    def close(): Unit = thisInput.close()
  }

  def flatMap[T](fn: Data => Seq[T]): Input[T] = new Input[T] {
    private var buf: Seq[T] = Nil
    private var cur = 0
    private var avail = 0

    def read(): Option[T] =
      if (cur == avail) {
        cur = 0
        avail = 0
        thisInput.read().map(fn) match {
          case None => None
          case Some(xs) =>
            if (xs.isEmpty) read()
            else if (xs.length == 1) xs.headOption
            else {
              avail = xs.length
              cur += 1
              buf = xs
              xs.headOption
            }
        }
      } else {
        cur += 1
        Some(buf(cur - 1))
      }

    def ready(): Boolean = cur < avail || thisInput.ready()

    def close(): Unit = {
      cur = 0
      avail = 0
      thisInput.close()
    }
  }

  override def foreach[U](fn: Data => U): Unit = {
    var next: Option[Data] = read()
    while (next.isDefined) {
      fn(next.get)
      next = read()
    }
  }
}

object Output {
  implicit def outputClosable[T]: Closable[Output[T]] = new Closable[Output[T]] {
    def close(out: Output[T]): Unit = out.close()
  }
}

trait Output[@specialized(Byte, Char) Data] {
  private var beingHandled = false

  def write(data: Data): Unit

  def writeBlock(array: Array[Data], offset: Int = 0, length: Int = -1): Int = {

    val end = if (length < 0) array.length - offset else offset + length
    array.slice(offset, end).foreach(write)

    end - offset
  }

  def flush(): Unit

  def close(): Unit
}

trait Reader_1 {
  implicit def stringByteReader(implicit encoding: Encoding): Reader[String, Byte] =
    new Reader[String, Byte] {
      def input(s: String): Input[Byte] = ByteArrayInput(s.getBytes(encoding.name))
    }

  implicit val stringLineReader: Reader[String, String] = StringLineReader
}

object Reader extends Reader_1 {
  implicit def inputStreamReader[T, I[T] <: Input[T]]: Reader[I[T], T] =
    new Reader[I[T], T] {
      def input(in: I[T]): Input[T] = in
    }

  implicit def byteInputToCharReader(implicit encoding: Encoding): Reader[Input[Char], Byte] =
    new Reader[Input[Char], Byte] {
      def input(in: Input[Char]): Input[Byte] = new Input[Byte] {
        private var cued: Array[Byte] = Array()
        private var index = 0
        def read(): Option[Byte] = {
          if (index >= cued.length) {
            // FIXME: Find a less stupid way of doing this
            val next = in.read()
            if (next.isEmpty) return None
            cued = next.get.toString.getBytes(encoding.name)
            index = 0
          }
          val next = cued(index)
          index += 1
          Some(next)
        }
        def ready(): Boolean = in.ready() || index < cued.length
        def close(): Unit = in.close()
      }
    }

  implicit def charInputToByteReader(implicit encoding: Encoding): Reader[Input[Byte], Char] =
    new Reader[Input[Byte], Char] {
      def input(in: Input[Byte]): Input[Char] = {
        val javaInputStream = new InputStream {
          def read(): Int = {
            val r = in.read()
            if (r.isDefined) r.get.toInt else -1
          }
        }
        alloc[CharInput](alloc[InputStreamReader](javaInputStream, encoding.name))
      }
    }

  implicit def byteToLineReaders[T](implicit jisr: JavaInputStreamReader[T], encoding: Encoding): Reader[T, String] =
    new Reader[T, String] {
      def input(t: T): Input[String] =
        alloc[LineInput](alloc[InputStreamReader](jisr.getInputStream(t)))
    }

  implicit def byteToCharReaders[T](implicit jisr: JavaInputStreamReader[T], encoding: Encoding): Reader[T, Char] =
    new Reader[T, Char] {
      def input(t: T): Input[Char] =
        alloc[CharInput](alloc[InputStreamReader](jisr.getInputStream(t)))
    }

  implicit def resourceBytes[Res](res: Res)(implicit sr: Reader[Res, Byte]): Bytes =
    slurpable(res).slurp[Byte]

  implicit val stringCharReader: Reader[String, Char] = StringCharReader
  implicit val byteArrayReader: Reader[Array[Byte], Byte] = ByteArrayReader
  implicit val bytesReader: Reader[Bytes, Byte] = BytesReader

  implicit val stdinReader: Reader[Stdin.type, Byte] = new Reader[Stdin.type, Byte] {
    def input(stdin: Stdin.type): Input[Byte] =
      ?[InputBuilder[InputStream, Byte]].input(System.in)
  }

}

@implicitNotFound(
    msg = "Cannot find implicit Reader for $"+"{Resource} resources. " +
        "$"+"{Resource} resources can only be read if a Reader implicit exists within scope. " +
        "Note that if you are working with Char data, you will require an implicit character " +
        "encoding, e.g. import encodings.system._ or import encodings.`UTF-8`._.")
trait Reader[-Resource, @specialized(Byte, Char) Data] {

  def input(res: Resource): Input[Data]

  def pump[DestResource](res: Resource, dest: DestResource)(implicit sw: Writer[DestResource, Data],
                                                            mf: ClassTag[Data]): Int =
    input(res) pumpTo sw.output(dest)
}

object StringCharReader extends Reader[String, Char] {
  def input(s: String): Input[Char] = StringIsInput(s)
}

object StringLineReader extends Reader[String, String] {
  def input(s: String): Input[String] = new Input[String] {
    private val lines = s.split("\n")
    private var cur = -1
    def ready() = cur < lines.length
    def close() = ()
    def read() = {
      cur += 1
      if (ready()) Some(lines(cur)) else None
    }
  }
}

object BytesReader extends Reader[Bytes, Byte] {
  def input(s: Bytes): Input[Byte] = ByteArrayInput(s.bytes)
}

object ByteArrayReader extends Reader[Array[Byte], Byte] {
  def input(s: Array[Byte]): Input[Byte] = ByteArrayInput(s)
}

object Stdin
object Stdout
object Stderr
