package com.monolito.kiros.auth.model

import com.roundeights.hasher.Hasher
import java.io._
import java.security._
import java.security.spec.PKCS8EncodedKeySpec
import java.security.spec.X509EncodedKeySpec
import javax.crypto._
import javax.crypto.spec.SecretKeySpec
import scala.io.Source

object Utils {
  val DEFAULT_TIME_LIFE = 60.0f

  def privateKey: String = {
    val filename = "private.key"
    val pkstr = Source.fromFile(filename).getLines() mkString "\n"

    val in = new FileInputStream(filename)
    var keyBytes = new Array[Byte](in.available())
    in.read(keyBytes)
    in.close()

    new String(keyBytes, "UTF-8")
  }

  val privateKeys = List(privateKey)

  def getHmac(value: String) = Hasher(value).hmac(privateKeys(0)).sha256.hex

  def verifyHmac(value: String, expected: String) =
    if (getHmac(value) == expected) true else false

  def decodeBase64(enc: String) =
    new sun.misc.BASE64Decoder().decodeBuffer(enc).toString()
}

case class AccessToken(access_token:String, token_type:String, scope:String, state:String)
