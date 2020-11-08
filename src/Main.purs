module Main where

import Prelude (map, show, ($), (<<<), (<>))

import Effect.Console (log)
import HTTPure as HTTPure
import Task (Priority(..), Tag(..), Task)
import View.Tasks (renderTasks)
import Data.Foldable (intercalate)

unlines :: Array String -> String
unlines = intercalate "\n"

tasks :: Array Task
tasks = [ { name: "Buy milk"
          , priority: Medium
          , tags: [Tag "home"]
          }
        , { name: "Call Doctor"
          , priority: High
          , tags: []
          }
        ]

main :: HTTPure.ServerM
main =
  HTTPure.serve 8080 router $ log "Server now up on port 8080"
  where
    router { path: [] } = HTTPure.permanentRedirect' (HTTPure.header "Location" "/tasks") ""
    router { path: ["tasks"] } = HTTPure.ok $ renderTasks tasks
    router _ = HTTPure.notFound
