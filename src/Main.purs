module Main where

import Prelude (map, show, ($), (<<<), (<>))

import Effect.Console (log)
import HTTPure as HTTPure
import Task (Priority(..), Tag(..), Task)
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

renderTask :: Task -> String
renderTask task = "Task name: " <> task.name <> ", priority: " <> show task.priority

renderTasks :: Array Task -> String
renderTasks = unlines <<< map renderTask

main :: HTTPure.ServerM
main =
  HTTPure.serve 8080 router $ log "Server now up on port 8080"
  where
    router { path: [] } = HTTPure.permanentRedirect' (HTTPure.header "Location" "/tasks") ""
    router { path: ["tasks"] } = HTTPure.ok $ renderTasks tasks
    router _ = HTTPure.notFound
