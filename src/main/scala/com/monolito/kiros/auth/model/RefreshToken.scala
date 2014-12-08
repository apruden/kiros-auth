package com.monolito.kiros.auth.model

case class RefreshToken(grantType: String, refreshToken: String, scope: String, clientId: String, clientSecret: String)
