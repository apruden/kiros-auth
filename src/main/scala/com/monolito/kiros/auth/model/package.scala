package com.monolito.kiros.auth

package object model {
  trait Entity {
    def getId: String

    def map: Map[String, Any] = Map()
  }
}
