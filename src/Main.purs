module Main where

import Prelude

import Effect.Console (log)
import Effect.Aff.Class (class MonadAff, liftAff)
import HTTPure as HTTPure
import HTTPure ((!@), Response)
import Task (Priority(..), Tag(..), Task)
import View.Tasks (renderTasks)
import Data.Foldable (intercalate)
import Node.FS.Aff as FS

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

serveStaticFile :: forall m. MonadAff m => String -> m Response
serveStaticFile path = liftAff $ FS.readFile path >>= HTTPure.ok

main :: HTTPure.ServerM
main =
  HTTPure.serve 8080 router $ log "Server now up on port 8080"
  where
    router { path: [] } = HTTPure.permanentRedirect' (HTTPure.header "Location" "/tasks") ""
    router { path: ["tasks"] } = HTTPure.ok $ renderTasks tasks
    router { path } | path !@ 0 == "static" = serveStaticFile (intercalate "/" path)
    router _ = HTTPure.notFound
