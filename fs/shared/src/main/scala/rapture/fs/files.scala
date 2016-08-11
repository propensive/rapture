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

package rapture.fs

import rapture.core._
import rapture.io._
import rapture.uri._

import java.io.{Reader => _, Writer => _, _}

trait FsMethods extends MethodConstraint

/** Provides support for accessing the file system through FsUrls. This is a wrapper for Java's
  * file handling facilities, and provides roughly the same functionality within the general URL
  * framework. */
trait LowerPriorityImplicits {

  /** Type class object for writing `FsUrl`s as `Output[Stream]`s */
  implicit object FileStreamCharWriter extends Writer[FsUrl, Char] {
    def output(url: FsUrl): Output[Char] =
      new CharOutput(new BufferedWriter(new FileWriter(url.javaFile)))
  }

  implicit object FileStreamCharAppender extends Appender[FsUrl, Char] {
    def appendOutput(url: FsUrl): Output[Char] =
      new CharOutput(new BufferedWriter(new FileWriter(url.javaFile, true)))
  }

  implicit val fileSizable: Sizable[FsUrl, Byte] = new Sizable[FsUrl, Byte] {

    /** Returns the size of the file in bytes. */
    def size(file: FsUrl): Long = file.length(modes.throwExceptions())
  }

  implicit val fileDeletable: Deleter[FsUrl] = new Deleter[FsUrl] {

    /** Returns the size of the file in bytes. */
    def delete(file: FsUrl): Unit = file.javaFile.delete()
  }
}

trait LowPriorityImplicits extends LowerPriorityImplicits {

  /** Type class object for writing `Byte`s to `FsUrl`s */
  implicit object FileStreamByteWriter extends Writer[FsUrl, Byte] {
    def output(url: FsUrl): Output[Byte] =
      new ByteOutput(new BufferedOutputStream(new FileOutputStream(url.javaFile)))
  }

  implicit object FileStreamByteAppender extends Appender[FsUrl, Byte] {
    def appendOutput(url: FsUrl): Output[Byte] =
      new ByteOutput(new BufferedOutputStream(new FileOutputStream(url.javaFile, true)))
  }

  implicit val fileMovable: Movable[FsUrl, FsUrl] = new Movable[FsUrl, FsUrl] {
    def move(from: FsUrl, to: FsUrl): Movable.Summary = {
      from.javaFile.renameTo(to.javaFile)
      Movable.Summary(None)
      // FIXME: Handle failure
    }
  }

  /*implicit class FileCopyable(f: FsUrl) {
    /** Renames this file to a new location. */
    def renameTo(dest: FsUrl): Boolean = f.javaFile.renameTo(dest.javaFile)
    
    /** Copies this file to a new location specified by the dest parameter. */
    def copyTo(dest: FsUrl, overwrite: Boolean = false, recursive: Boolean = false)
        (implicit sr: Reader[FsUrl, Byte], mode: Mode[FsMethods]):
        mode.Wrap[Int, Exception] = mode.wrap {
      if(dest.exists) {
        if(f.isFile && !dest.isFile) throw new Exception("Cannot copy a file onto a directory")
        else if(!f.isFile && dest.isFile) throw new Exception("Cannot copy a directory onto a file")
        else if(!overwrite) throw new Exception("Destination already exists")
        else if(f.isFile) sr.pump(f, dest)
        else if(!recursive) throw new Exception("Cannot copy directory")
        else NavigableFile.children(f)(raw).foldLeft(0) { (c, f2) =>
          implicit val eh = raw
          c + f2.copyTo(dest / f2.filename, overwrite, recursive)
        }
      } else {
        if(f.isFile) sr.pump(f, dest) else {
          dest.mkdir()
          NavigableFile.children(f)(raw).foldLeft(0) { (c, f2) =>
            implicit val eh = raw
            c + f2.copyTo(dest / f2.filename, overwrite, recursive)
          }
        }
      }
    }

    /** Moves this file to a new location specified by the dest parameter. This will first attempt
   * to move the file by renaming it, but will attempt copying and deletion if renaming fails. */
    def moveTo(dest: FsUrl)(implicit sr: Reader[FsUrl, Byte], mode: Mode[FsMethods]):
        mode.Wrap[Boolean, Exception] =
      mode.wrap(renameTo(dest) || (copyTo(dest)(sr, raw) > 0) && delete()(raw))

    /** Deletes the file represented by this FsUrl. If the recursive flag is set and the
   * filesystem object is a directory, all subfolders and their contents will also be
   * deleted. */
    def delete(recursive: Boolean = false)(implicit mode: Mode[FsMethods]):
        mode.Wrap[Boolean, Exception] = mode.wrap(if(recursive) deleteRecursively(f)
        else f.javaFile.delete())
    
    private def deleteRecursively(file: FsUrl): Boolean = {
      if(NavigableFile.isDirectory(file)(raw))
        NavigableFile.children(file)(raw).foreach(deleteRecursively)
      
      delete()(raw)
    }
  }*/

  /** Specifies how file: URLs should be navigable. */
  implicit object NavigableFile extends Navigable[FsUrl] {
    def children(url: FsUrl)(implicit mode: Mode[`Navigable#children`]): mode.Wrap[List[FsUrl], Exception] =
      mode.wrap { if (url.isFile) Nil else url.javaFile.list().to[List].map(url / _) }

    def isDirectory(url: FsUrl)(implicit mode: Mode[`Navigable#isDirectory`]): mode.Wrap[Boolean, Exception] =
      mode.wrap { url.javaFile.isDirectory() }
  }

}

object `package` extends LowPriorityImplicits {

  /** Type class object for reading `Byte`s from `FsUrl`s */
  implicit object FileStreamByteReader extends JavaInputStreamReader[FsUrl](f => new FileInputStream(f.javaFile))

  implicit class EnrichedFileUriContext(uc: UriContext.type) {
    def file(constants: List[String])(variables: List[String]) = {
      val fileUrl =
        constants.zip(variables :+ "").map { case (a, b) => a + b }.mkString.split("/").filter(_ != "").to[Vector]
      FsUrl(fileUrl)
    }
  }
}

object FsUrl {
  implicit val hasResourceName: HasResourceName[FsUrl] = new HasResourceName[FsUrl] {
    def resourceName(fsUrl: FsUrl): String = fsUrl.filename
  }
  
  implicit val fileCpUrl: ClasspathUrlable[FsUrl] = new ClasspathUrlable[FsUrl] {
    def toClasspathUrlItem(f: FsUrl) = ClasspathUrlItem(List(new java.net.URL(f.toString)))
  }

  implicit def uriCapable: UriCapable[FsUrl] = new UriCapable[FsUrl] {
    def uri(f: FsUrl): Uri =
      Uri("file", f.elements.mkString("///", "/", ""))
  }

  implicit def fileSlashString: Dereferenceable[FsUrl, String, FsUrl] =
    new Dereferenceable[FsUrl, String, FsUrl] {
      def dereference(p1: FsUrl, p2: String) = {
        val start = if (p1.elements.lastOption == Some("")) p1.elements.init else p1.elements
        FsUrl(start :+ p2)
      }
    }

  implicit def fileSlashRelativePath[RP <: RelativePath]: Dereferenceable[FsUrl, RP, FsUrl] =
    new Dereferenceable[FsUrl, RP, FsUrl] {
      def dereference(p1: FsUrl, p2: RP) = FsUrl(p1.elements.dropRight(p2.ascent) ++ p2.elements)
    }

  implicit def fileSlashRootedPath[RP <: RootedPath]: Dereferenceable[FsUrl, RP, FsUrl] =
    new Dereferenceable[FsUrl, RP, FsUrl] {
      def dereference(p1: FsUrl, p2: RP) = {
        val start = if (p1.elements.lastOption == Some("")) p1.elements.init else p1.elements
        FsUrl(start ++ p2.elements)
      }
    }

  implicit def fileParentable: Parentable[FsUrl, FsUrl] = new Parentable[FsUrl, FsUrl] {
    def parent(fsUrl: FsUrl): FsUrl = FsUrl(fsUrl.elements.dropRight(1))
  }
}

/** Defines a URL for the file: scheme, and provides standard filesystem operations on the file
  * represented by the URL. */
case class FsUrl(elements: Seq[String]) {

  override def toString = s"file:///${elements.mkString("/")}"

  /** The java.io.File corresponding to this FsUrl. */
  lazy val javaFile: java.io.File = new java.io.File(this.uri.schemeSpecificPart.drop(2))

  /** Returns true if the file or directory represented by this FsUrl can be read from. */
  def readable: Boolean = javaFile.canRead()

  /** Returns true if the file or directory represented by this FsUrl can be written to. */
  def writable: Boolean = javaFile.canWrite()

  /** Add a hook to the filesystem to delete this file upon shutdown of the JVM. */
  def deleteOnExit(): Unit = javaFile.deleteOnExit()

  /** Returns true if this object exists on the filesystem. */
  def exists: Boolean = javaFile.exists()

  /** Returns the filename of this filesystem object. */
  def filename: String = javaFile.getName()

  /** Returns true if the filesystem object represented by this FsUrl is a file, and false if
    * it is a directory. */
  def isFile: Boolean = javaFile.isFile()

  /** Returns true if the file or directory is hidden. */
  def hidden: Boolean = if (exists) javaFile.isHidden() else throw new Exception()

  /** Returns the date of the last modification to the file or directory. */
  def lastModified[I: TimeSystem.ByInstant](implicit mode: Mode[FsMethods]): mode.Wrap[I, Exception] =
    mode.wrap(javaFile.lastModified() match {
      case 0L => throw new Exception()
      case d => ?[TimeSystem.ByInstant[I]].instant(d)
    })

  /** Returns the size of the file in bytes. */
  def length(implicit mode: Mode[FsMethods]): mode.Wrap[Long, Exception] =
    mode.wrap(javaFile.length() match {
      case 0L if !exists => throw new Exception()
      case x => x
    })

  /** If the filesystem object represented by this FsUrl does not exist, it is created as a
    * directory, provided that either the immediate parent directory already exists, or the
    * makeParents path is set. */
  def mkdir(makeParents: Boolean = false)(implicit mode: Mode[FsMethods]): mode.Wrap[Boolean, Exception] =
    mode.wrap(
        if (makeParents) javaFile.mkdirs()
        else
          javaFile.mkdir())

  /** Extract the file extension from the name of this file. */
  def extension(implicit mode: Mode[FsMethods]): mode.Wrap[Option[String], Exception] =
    mode.wrap(if (filename contains ".") Some(filename.split("\\.").last) else None)
}

/** The file scheme object used as a factory for FsUrls. */
object File extends FsUrl(Vector()) {

  private val UrlRegex = """^file:///(.*)$""".r

  /** Pares a path to a file */
  def parse(s: String) = s match {
    case UrlRegex(path) => FsUrl(path.split("\\/").to[Vector])
    case path => FsUrl(path.split("\\/").to[Vector].dropWhile(_ == ""))
  }

  def homeDir = File.parse(System.getenv("HOME"))
}
