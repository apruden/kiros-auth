package com.monolito.kiros.auth

import java.security.{ SecureRandom, KeyStore }
import javax.net.ssl.{ KeyManagerFactory, SSLContext, TrustManagerFactory }
import spray.io._
import spray.io.ServerSSLEngineProvider
import java.io.FileInputStream

trait AuthSslConfiguration {

  implicit def sslEngineProvider = ServerSSLEngineProvider { engine =>
    engine.setEnabledCipherSuites(Array("TLS_RSA_WITH_AES_256_CBC_SHA"))
    engine.setEnabledProtocols(Array("SSLv3", "TLSv1"))
    engine
  }

  //Generate self-signed certificate:
  //keytool -genkey -keyalg RSA -alias selfsigned -keystore keystore.jks -storepass password -validity 360 -keysize 2048
  // if there is no SSLContext in scope implicitly the HttpServer uses the default SSLContext,
  // since we want non-default settings in this example we make a custom SSLContext available here
  implicit def sslContext: SSLContext = {
    val keyStore = KeyStore.getInstance(KeyStore.getDefaultType())
    keyStore.load(new FileInputStream("keystore.jks"), "Admin123".toCharArray) //file location: ./keystore.jks
    val kmf = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm())
    kmf.init(keyStore, "Admin123".toCharArray)
    val tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm())
    tmf.init(keyStore)
    val context = SSLContext.getInstance("TLS")
    context.init(kmf.getKeyManagers(), tmf.getTrustManagers(), null)
    context
  }
}