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

package rapture.core

private[core] trait DefaultsTo_1 {
  implicit def fallbackDefaultsTo[T, S]: DefaultsTo[T, S] = null.asInstanceOf[DefaultsTo[T, S]]
}

object DefaultsTo extends DefaultsTo_1 {
  implicit def defaultDefaultsTo[T]: DefaultsTo[T, T] = null.asInstanceOf[DefaultsTo[T, T]]
}

trait DefaultsTo[T, S]
