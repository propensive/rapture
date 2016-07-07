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

package rapture.crypto
import rapture.core._
import rapture.codec._

import java.security._
import javax.crypto.Mac

trait DigestType
trait Sha1 extends DigestType
trait Sha256 extends DigestType
trait Sha384 extends DigestType
trait Sha512 extends DigestType
trait Md2 extends DigestType
trait Md5 extends DigestType

class Digest[T <: DigestType](bytes: Array[Byte]) extends Bytes(bytes)

package ciphers {
  object des {
    implicit def desGenerator: KeyGenerator[Des] = Des.keyGenerator
    implicit def desDecryption = Des.decryption
    implicit def desEncryption = Des.encryption
  }

  object blowfish {
    implicit def blowfishGenerator: KeyGenerator[Blowfish] = Blowfish.keyGenerator
    implicit def blowfishDecryption = Blowfish.decryption
    implicit def blowfishEncryption = Blowfish.encryption
  }

  object aes {
    implicit def aesGenerator: KeyGenerator[Aes] = Aes.keyGenerator
    implicit def aesDecryption = Aes.decryption
    implicit def aesEncryption = Aes.encryption
  }

}

class EncryptedData[C <: CipherType](bytes: Array[Byte]) extends Bytes(bytes)

object Hash {
  def digest[D <: DigestType: Digester](msg: Bytes): Digest[D] =
    new Digest[D](?[Digester[D]].digest(msg.bytes))
}

object Digester {
  implicit val sha1: Digester[Sha1] = digests.sha1
  implicit val sha256: Digester[Sha256] = digests.sha256
  implicit val sha512: Digester[Sha512] = digests.sha512
  implicit val sha384: Digester[Sha384] = digests.sha384
  implicit val md5: Digester[Md5] = digests.md5
  implicit val md2: Digester[Md2] = digests.md2
}
abstract class Digester[D <: DigestType] {

  /** Digests the array of bytes. */
  def digest(msg: Array[Byte]): Array[Byte]
}

case class Salt(value: String)

object Password {
  def apply(value: String)(implicit salt: Salt) = new HashedPassword(value)(salt)
}

class Password(private val value: String)(implicit salt: Salt) {
  def digest: String = Bytes(Digester.sha256.digest((value + salt).getBytes("UTF-8"))).encode[Hex]
  override def toString = s"password:$digest"
  def check(password: String) = new Password(password).digest == digest
}

class HashedPassword(hash: String)(implicit salt: Salt) extends Password(null)(salt) {
  override def digest: String = hash
}

object digests {

  implicit val sha1: Digester[Sha1] = new Digester[Sha1] {
    def digest(msg: Array[Byte]): Array[Byte] =
      MessageDigest.getInstance("SHA-1").digest(msg)
  }

  /** SHA-256 digester, with additional methods for secure password encoding. */
  implicit val sha256: Digester[Sha256] = new Digester[Sha256] {

    /** Digests the given bytes. */
    def digest(msg: Array[Byte]): Array[Byte] =
      MessageDigest.getInstance("SHA-256").digest(msg)
  }

  /** SHA-512 digester, with additional methods for secure password encoding. */
  implicit val sha512: Digester[Sha512] = new Digester[Sha512] {
    def digest(msg: Array[Byte]): Array[Byte] =
      MessageDigest.getInstance("SHA-512").digest(msg)
  }

  /** SHA-384 digester, with additional methods for secure password encoding. */
  implicit val sha384: Digester[Sha384] = new Digester[Sha384] {
    def digest(msg: Array[Byte]): Array[Byte] =
      MessageDigest.getInstance("SHA-384").digest(msg)
  }

  /** MD5 Digester. This is included for backwards compatibility. MD5 is no
    * longer considered future-proof and new designs should prefer SHA-256. */
  implicit val md5: Digester[Md5] = new Digester[Md5] {
    def digest(msg: Array[Byte]): Array[Byte] =
      MessageDigest.getInstance("MD5").digest(msg)
  }

  implicit val md2: Digester[Md2] = new Digester[Md2] {
    def digest(msg: Array[Byte]): Array[Byte] =
      MessageDigest.getInstance("MD2").digest(msg)
  }
}

trait CipherType
trait Blowfish extends CipherType

class JavaxCryptoImplementations[Codec <: CipherType](codec: String) {
  implicit def encryption: Encryption[Codec, Bytes] = new Encryption[Codec, Bytes] {
    def encrypt(key: Array[Byte], message: Bytes) = {
      val cipher = javax.crypto.Cipher.getInstance(codec)
      cipher.init(javax.crypto.Cipher.ENCRYPT_MODE, new javax.crypto.spec.SecretKeySpec(key, codec))
      cipher.doFinal(message.bytes)
    }
  }

  implicit def decryption = new Decryption[Codec] {
    def decrypt(key: Array[Byte], message: Array[Byte]) = {
      val cipher = javax.crypto.Cipher.getInstance(codec)
      cipher.init(javax.crypto.Cipher.DECRYPT_MODE, new javax.crypto.spec.SecretKeySpec(key, codec))
      cipher.doFinal(message)
    }
  }

  implicit def keyGenerator: KeyGenerator[Codec] = new KeyGenerator[Codec] {
    def generate(): Array[Byte] = {
      val keyGen = javax.crypto.KeyGenerator.getInstance(codec)
      keyGen.generateKey().getEncoded
    }
  }
}

trait Aes extends CipherType
object Aes extends JavaxCryptoImplementations[Aes]("AES")
object Des extends JavaxCryptoImplementations[Des]("DES")
object Blowfish extends JavaxCryptoImplementations[Blowfish]("Blowfish")

trait TripleDes extends CipherType
trait Des extends CipherType

trait KeyGenerator[K <: CipherType] {
  type KeyType = K
  def generate(): Array[Byte]
}

trait Encryption[-C <: CipherType, Msg] {
  def encrypt(key: Array[Byte], message: Msg): Array[Byte]
}

case class DecryptionException() extends Exception

trait Decryption[C <: CipherType] {
  def decrypt(key: Array[Byte], message: Array[Byte]): Array[Byte]
}

object Key {
  def generate[K <: CipherType]()(implicit gen: KeyGenerator[K]): Key[gen.KeyType] =
    new Key[gen.KeyType](?[KeyGenerator[K]].generate())

  def read[K <: CipherType](key: Bytes): Key[K] =
    new Key[K](key.bytes)
}

class Key[C <: CipherType](bytes: Array[Byte]) extends Bytes(bytes) {
  def encrypt[Msg](message: Msg)(implicit encryption: Encryption[C, Msg]): EncryptedData[C] =
    new EncryptedData[C](encryption.encrypt(bytes, message))

  def decrypt(message: EncryptedData[C])(implicit mode: Mode[`Key#decrypt`],
                                         decryption: Decryption[C]): mode.Wrap[Bytes, DecryptionException] =
    mode wrap {
      try Bytes(decryption.decrypt(bytes, message.bytes))
      catch {
        case e: Exception => mode.exception(DecryptionException())
      }
    }
}

case class HmacSigner(key: Bytes) {
  type Sha256Hmac <: DigestType
  implicit val hmac: Digester[Sha256Hmac] = new Digester[Sha256Hmac] {
    def digest(msg: Array[Byte]): Array[Byte] = {
      val mac = Mac.getInstance("HmacSHA256")
      val secretKey = new javax.crypto.spec.SecretKeySpec(key.bytes, "HmacSHA256")
      mac.init(secretKey)
      mac.doFinal(msg)
    }
  }

}
