/******************************************************************************************************************\
* Rapture Core, version 2.0.0. Copyright 2010-2015 Jon Pretty, Propensive Ltd.                                     *
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
package rapture.core

import java.lang.{ClassLoader => JClassLoader, Thread => JThread}

object ClasspathUrlItem {
  implicit def toClasspathUrlItem[T: ClasspathUrlable](t: T): ClasspathUrlItem =
    ?[ClasspathUrlable[T]].toClasspathUrlItem(t)
}

case class ClasspathUrlItem(javaUrl: List[java.net.URL])

object ClasspathUrlable {
  implicit def seqUrlable[T](implicit urlable: ClasspathUrlable[T]): ClasspathUrlable[List[T]] =
    new ClasspathUrlable[List[T]] {
      def toClasspathUrlItem(xs: List[T]): ClasspathUrlItem =
        ClasspathUrlItem(xs.flatMap(urlable.toClasspathUrlItem(_).javaUrl))
    }
}
trait ClasspathUrlable[T] { def toClasspathUrlItem(t: T): ClasspathUrlItem }

object ClassLoader {
  implicit def defaultClassLoader: ClassLoader =
    new ClassLoader(JThread.currentThread.getContextClassLoader)

  def apply(urls: ClasspathUrlItem*): ClassLoader =
    new ClassLoader(new java.net.URLClassLoader(urls.flatMap(_.javaUrl).to[Array]))
}

class ClassLoader(val javaClassLoader: JClassLoader) {
  def applyTo[T](blk: => T): T = {
    val cur = java.lang.Thread.currentThread().getContextClassLoader
    java.lang.Thread.currentThread().setContextClassLoader(javaClassLoader)
    val result = blk
    java.lang.Thread.currentThread().setContextClassLoader(cur)
    result
  }
}

object Thread {
  def fork(threadName: String, daemon: Boolean = false)(blk: => Unit)
      (implicit cl: ClassLoader): Thread =
    ThreadSpec(threadName, daemon)(blk).spawn()

  def sleep[D: TimeSystem.ByDuration](duration: D) =
    JThread.sleep(?[TimeSystem.ByDuration[D]].fromDuration(duration))

}

case class ThreadSpec(name: String, daemon: Boolean = false)(blk: => Unit)
    (implicit cl: ClassLoader) {

  def spawn(): Thread = {
    val parentThread = JThread.currentThread
    val javaThread = new JThread(name) {
      override def run() = {
        blk
        parentThread.join()
      }
    }
    javaThread.setDaemon(daemon)
    javaThread.setContextClassLoader(cl.javaClassLoader)
    javaThread.start()
    
    new Thread(this, javaThread) {
      def parentAlive = javaThread.isAlive
    }
  }
}

abstract class Thread(spec: ThreadSpec, javaThread: JThread) {
  def daemon: Boolean = spec.daemon
  def name: String = spec.name

  def alive: Boolean = javaThread.isAlive
  def interrupt(): Unit = javaThread.interrupt()
  def join(): Unit = javaThread.join()

  def priority = javaThread.getPriority
  def priority_=(p: Int) = javaThread.setPriority(p)

  override def toString = s"[$name]"
}
