/******************************************************************************************************************\
* Rapture FS, version 2.0.0. Copyright 2010-2015 Jon Pretty, Propensive Ltd.                                       *
*                                                                                                                  *
* The primary distribution site is http://rapture.io/                                                              *
*                                                                                                                  *
* Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in complance    *
* with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.            *
*                                                                                                                  *
* Unless required by applicable law or agreed to in writing, software distributed under the License is distributed *
* on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License    *
* for the specific language governing permissions and limitations under the License.                               *
\******************************************************************************************************************/
package rapture.fs

import rapture.core._
import rapture.io._
import rapture.uri._

import java.io.{Reader => _, Writer => _, _}
import java.util.regex.Pattern

trait FsMethods extends MethodConstraint

/** Provides support for accessing the file system through FileUrls. This is a wrapper for Java's
  * file handling facilities, and provides roughly the same functionality within the general URL
  * framework. */
trait LowerPriorityImplicits {

  /** Type class object for writing `FileUrl`s as `Output[Stream]`s */
  implicit object FileStreamCharWriter extends Writer[FileUrl, Char] {
    def output(url: FileUrl): Output[Char] =
       new CharOutput(new BufferedWriter(new FileWriter(url.javaFile)))
  }

  implicit object FileStreamCharAppender extends Appender[FileUrl, Char] {
    def appendOutput(url: FileUrl): Output[Char] =
      new CharOutput(new BufferedWriter(new FileWriter(url.javaFile, true)))
  }

  implicit val fileSizable: Sizable[FileUrl, Byte] = new Sizable[FileUrl, Byte] {
    /** Returns the size of the file in bytes. */
    def size(file: FileUrl): Long = file.length(modes.throwExceptions())
  }
  
  implicit val fileDeletable: Deleter[FileUrl] = new Deleter[FileUrl] {
    /** Returns the size of the file in bytes. */
    def delete(file: FileUrl): Unit = file.javaFile.delete()
  }
}

trait LowPriorityImplicits extends LowerPriorityImplicits {
  /** Type class object for writing `Byte`s to `FileUrl`s */
  implicit object FileStreamByteWriter extends Writer[FileUrl, Byte] {
    def output(url: FileUrl): Output[Byte] =
      new ByteOutput(new BufferedOutputStream(new FileOutputStream(url.javaFile)))
  }

  implicit object FileStreamByteAppender extends Appender[FileUrl, Byte] {
    def appendOutput(url: FileUrl): Output[Byte] =
      new ByteOutput(new BufferedOutputStream(new FileOutputStream(url.javaFile, true)))
  }

  implicit val fileMovable: Movable[FileUrl, FileUrl] = new Movable[FileUrl, FileUrl] {
    def move(from: FileUrl, to: FileUrl): Movable.Summary = {
      from.javaFile.renameTo(to.javaFile)
      Movable.Summary(None)
      // FIXME: Handle failure
    }
  }

  /*implicit class FileCopyable(f: FileUrl) {
    /** Renames this file to a new location. */
    def renameTo(dest: FileUrl): Boolean = f.javaFile.renameTo(dest.javaFile)
    
    /** Copies this file to a new location specified by the dest parameter. */
    def copyTo(dest: FileUrl, overwrite: Boolean = false, recursive: Boolean = false)
        (implicit sr: Reader[FileUrl, Byte], mode: Mode[FsMethods]):
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
    def moveTo(dest: FileUrl)(implicit sr: Reader[FileUrl, Byte], mode: Mode[FsMethods]):
        mode.Wrap[Boolean, Exception] =
      mode.wrap(renameTo(dest) || (copyTo(dest)(sr, raw) > 0) && delete()(raw))

    /** Deletes the file represented by this FileUrl. If the recursive flag is set and the
      * filesystem object is a directory, all subfolders and their contents will also be
      * deleted. */
    def delete(recursive: Boolean = false)(implicit mode: Mode[FsMethods]):
        mode.Wrap[Boolean, Exception] = mode.wrap(if(recursive) deleteRecursively(f)
        else f.javaFile.delete())
    
    private def deleteRecursively(file: FileUrl): Boolean = {
      if(NavigableFile.isDirectory(file)(raw))
        NavigableFile.children(file)(raw).foreach(deleteRecursively)
      
      delete()(raw)
    }
  }*/

  /** Specifies how file: URLs should be navigable. */
  implicit object NavigableFile extends Navigable[FileUrl] {
    def children(url: FileUrl)(implicit mode: Mode[`Navigable#children`]): mode.Wrap[List[FileUrl], Exception] = 
      mode.wrap { if(url.isFile) Nil else url.javaFile.list().to[List].map(url / _) }
    
    def isDirectory(url: FileUrl)(implicit mode: Mode[`Navigable#isDirectory`]): mode.Wrap[Boolean, Exception] =
      mode.wrap { url.javaFile.isDirectory() }
  }

}

object `package` extends LowPriorityImplicits {
  /** Type class object for reading `Byte`s from `FileUrl`s */
  implicit object FileStreamByteReader extends JavaInputStreamReader[FileUrl](f =>
      new FileInputStream(f.javaFile))

  implicit class EnrichedFileUriContext(uc: UriContext.type) {
    def file(constants: List[String])(variables: List[String]) = {
      val fileUrl = constants.zip(variables :+ "").map { case (a, b) => a+b }.mkString.split("/").filter(_ != "").to[Vector]
      FileUrl(fileUrl)
    }
  }
}

object FileUrl {
  implicit val fileCpUrl: ClasspathUrlable[FileUrl] = new ClasspathUrlable[FileUrl] {
    def toClasspathUrlItem(f: FileUrl) = ClasspathUrlItem(List(new java.net.URL(f.toString)))
  }

  implicit def uriCapable: UriCapable[FileUrl] = new UriCapable[FileUrl] {
    def uri(f: FileUrl): Uri =
      Uri("file", f.elements.mkString("//", "/", ""))
  }

  implicit def fileSlashString: Dereferenceable[FileUrl, String, FileUrl] =
    new Dereferenceable[FileUrl, String, FileUrl] {
      def dereference(p1: FileUrl, p2: String) = FileUrl(p1.elements :+ p2)
    }

  implicit def fileSlashRelativePath[RP <: RelativePath]: Dereferenceable[FileUrl, RP, FileUrl] =
    new Dereferenceable[FileUrl, RP, FileUrl] {
      def dereference(p1: FileUrl, p2: RP) = FileUrl(p1.elements.dropRight(p2.ascent) ++ p2.elements)
    }
}

/** Defines a URL for the file: scheme, and provides standard filesystem operations on the file
  * represented by the URL. */
case class FileUrl(val elements: Seq[String]) {

  /** The java.io.File corresponding to this FileUrl. */
  lazy val javaFile: java.io.File = new java.io.File(this.uri.schemeSpecificPart.drop(2))
  
  /** Returns true if the file or directory represented by this FileUrl can be read from. */
  def readable: Boolean = javaFile.canRead()
 
  /** Returns true if the file or directory represented by this FileUrl can be written to. */
  def writable: Boolean = javaFile.canWrite()
  
  /** Add a hook to the filesystem to delete this file upon shutdown of the JVM. */
  def deleteOnExit(): Unit = javaFile.deleteOnExit()
  
  /** Returns true if this object exists on the filesystem. */
  def exists: Boolean = javaFile.exists()
  
  /** Returns the filename of this filesystem object. */
  def filename: String = javaFile.getName()
  
  /** Returns true if the filesystem object represented by this FileUrl is a file, and false if
    * it is a directory. */
  def isFile: Boolean = javaFile.isFile()
  
  /** Returns true if the file or directory is hidden. */
  def hidden: Boolean = if(exists) javaFile.isHidden() else throw new Exception()
 
  /** Returns the date of the last modification to the file or directory. */
  def lastModified[I: TimeSystem.ByInstant](implicit mode: Mode[FsMethods]):
      mode.Wrap[I, Exception] =
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
  
  /** If the filesystem object represented by this FileUrl does not exist, it is created as a
    * directory, provided that either the immediate parent directory already exists, or the
    * makeParents path is set. */
  def mkdir(makeParents: Boolean = false)(implicit mode: Mode[FsMethods]):
      mode.Wrap[Boolean, Exception] = mode.wrap(if(makeParents) javaFile.mkdirs() else
      javaFile.mkdir())
  
  /** Extract the file extension from the name of this file. */
  def extension(implicit mode: Mode[FsMethods]): mode.Wrap[Option[String], Exception] =
    mode.wrap(if(filename contains ".") Some(filename.split("\\.").last) else None)
}

/** The file scheme object used as a factory for FileUrls. */
object File extends FileUrl(Vector()) {

  private val UrlRegex = """^file://(/.*)$""".r

  /** Pares a path to a file */
  def parse(s: String) = s match {
    case UrlRegex(path) => FileUrl(path.split("\\/").to[Vector])
  }

  def home = File.parse(System.getenv("HOME"))
}

