module Main where

import Prelude

import Effect.Console (log)
import HTTPure as HTTPure

main :: HTTPure.ServerM
main =
  HTTPure.serve 8080 router $ log "Server now up on port 8080"
  where
    router _ = HTTPure.ok "hello world!"
