module Main where

import Prelude

import Data.Tuple (Tuple(..))
import Effect.Aff (launchAff_)
import Effect.Console (log)
import HTTPure as HTTPure
import Model.User
import Persistence (saveUser)
import Persistence.InMemoryTaskRepository (mkInMemoryPersitence)
import Persistence.InMemoryUserRepository (mkInMemoryUserRepository)
import Router (router)

main :: HTTPure.ServerM
main = do
  taskRepo <- mkInMemoryPersitence
  userRepo <- mkInMemoryUserRepository
  let env = Tuple userRepo taskRepo
      user = { id: (UserId "12345")
             , username: (UserName "tim")
             , password: "password"
             }
  launchAff_ $ saveUser userRepo user
  HTTPure.serve 8080 (router env) $ log "Server now up on port 8080"
