package com.monolito.kiros.auth

import com.monolito.kiros.auth.data.ClientRepository
import com.monolito.kiros.auth.data.UserRepository

case class AppContext (clients: ClientRepository, users: UserRepository)