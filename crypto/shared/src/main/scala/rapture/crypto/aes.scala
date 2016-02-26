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
package rapture.crypto

import rapture.core._

import javax.crypto.spec._
import java.util._

import digests._

trait `AesEncryption#decrypt` extends MethodConstraint
trait `Key#decrypt` extends MethodConstraint

/** Provides a simple interface for AES encryption with SHA-256 digest
  * verification. This class is stateless. */
abstract class AesEncryption {

  /** Must be 16, 24 or 32 bytes long. */
  protected def secretKey: Array[Byte]

  private val keySpec = new SecretKeySpec(secretKey, "AES")

  def encrypt(clearText: Array[Byte], iv: Array[Byte] = null): Array[Byte] = {
    
    val cipher = javax.crypto.Cipher.getInstance("AES/CBC/PKCS5Padding")
    
    if(iv == null) cipher.init(javax.crypto.Cipher.ENCRYPT_MODE, keySpec)
    else cipher.init(javax.crypto.Cipher.ENCRYPT_MODE, keySpec, new IvParameterSpec(iv))
    
    val digest = Hash.digest[Sha256](clearText).bytes
    val paddedLength = (clearText.length >> 4) + 1 << 4
    val cipherText = new Array[Byte](paddedLength + (if(iv == null) 48 else 0))
    
    if(iv == null) {
      Array.copy(cipher.getIV, 0, cipherText, 0, 16)
      cipher.update(digest, 0, 32, cipherText, 16)
    }
    cipher.doFinal(clearText, 0, clearText.length, cipherText, if(iv == null) 48 else 0)
    
    cipherText
  }

  def decrypt(cipherText: Array[Byte], iv: Array[Byte] = null)(implicit mode: Mode[`AesEncryption#decrypt`]):
      mode.Wrap[Array[Byte], DecryptionException] = mode.wrap {
    if(iv == null && cipherText.length < 48) mode.exception(DecryptionException())
      
    val cipher = javax.crypto.Cipher.getInstance("AES/CBC/PKCS5Padding")
    val ips = if(iv == null) new IvParameterSpec(cipherText, 0, 16) else new IvParameterSpec(iv)
    
    cipher.init(javax.crypto.Cipher.DECRYPT_MODE, keySpec, ips)
    
    val n = if(iv == null) 64 else 0
    
    val digest1 = if(iv == null) cipher.update(cipherText, 16, 48) else Array[Byte]()
    val clearText = cipher.doFinal(cipherText, n, cipherText.length - n)
    
    if(iv == null) {
      val digest2 = Hash.digest[Sha256](clearText).bytes
      var i = 0
      var r = true
    
      
      while(i < 32) {
        if(digest1(i) != digest2(i)) r = false
        i += 1
      }

      if(!r) {
        Arrays.fill(digest1, 0.toByte)
        Arrays.fill(digest2, 0.toByte)
        Arrays.fill(clearText, 0.toByte)
        mode.exception(DecryptionException())
      }
    }
    
    clearText
  }

  def apply(clearText: Array[Byte]): Array[Byte] = encrypt(clearText)
  
  def unapply(cipherText: Array[Byte]): Option[Array[Byte]] =
    try Some(decrypt(cipherText)) catch { case DecryptionException() => None }
}
