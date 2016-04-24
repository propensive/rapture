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

object MinimumEditDistance {
  def difference(a: String, b: String): Int = {
    var d = Vector.fill(a.length + 1)(Vector.fill(b.length + 1)(0))
    (0 to b.length).foreach { j =>
      (0 to a.length).foreach { i =>
        if(i == 0 || j == 0) d = d.updated(i, d(i).updated(j, i + j))
        else if(a(i - 1) == b(j - 1)) d = d.updated(i, d(i).updated(j, d(i - 1)(j - 1)))
        else d = d.updated(i, d(i).updated(j, List(d(i - 1)(j), d(i)(j - 1), d(i - 1)(j - 1)).min + 1))
      }
    }
    d(a.length)(b.length)
  }
 
  def filterStrings(words: Array[String], word: String, limit: Int): List[String] = {
    val arr = new Array[Int](256)
    val results = new collection.mutable.ListBuffer[String]
    
    for(i <- 0 to 15) {
      arr(i) = i
      arr(16*i) = i
    }

    def difference(d: Array[Int], a: String): Int = {
      val amax = math.min(a.length, 15)
      val t = amax + word.length
      var i, j, n = 0
      var min = Int.MaxValue
      var cont = true
      while(n <= t && cont) {
        val r = if(i == 0 || j == 0) i + j
          else if(a(i - 1) == word(j - 1)) d(16*(i - 1) + j - 1)
          else math.min(math.min(d(16*(i - 1) + j), d(16*i + j - 1)), d(16*(i - 1) + j - 1)) + 1
	
	min = math.min(min, r)
	d(16*i + j) = r
      
	if(j == 0 || i == amax) {
	  n += 1
	  if(n <= word.length) { j = n; i = 0 }
	  else { i = n - word.length; j = word.length }
	  if(min > limit) cont = false
	  min = Int.MaxValue
	} else {
	  i += 1
	  j -= 1
	}
      }
      
      if(cont) d(16*amax + word.length) else Int.MaxValue
    }
    
    val len = word.length

    words.filter { w =>
      difference(arr, w.take(len)) <= limit
    }.to[List]
  }
}
