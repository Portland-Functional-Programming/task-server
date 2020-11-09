module Main where

import Prelude

import Effect.Console (log)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import HTTPure as HTTPure
import HTTPure ((!@), Response)
import Task (Priority(..), Tag(..), Task)
import View.Tasks (renderTasks)
import Data.Array (tail)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Node.FS.Aff as FS

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

serveStaticFile :: forall m. MonadAff m => String -> m Response
serveStaticFile path = liftAff $ FS.readFile path >>= HTTPure.ok

main :: HTTPure.ServerM
main =
  HTTPure.serve 8080 router $ log "Server now up on port 8080"
  where
    router { path: [] } = HTTPure.permanentRedirect' (HTTPure.header "Location" "/tasks") ""
    router { path: ["tasks"] } = HTTPure.ok $ renderTasks tasks
    router { path } | path !@ 0 == "static" = (liftEffect $ log ("path: " <> show path)) *> (serveStaticFile (intercalate "/" path))
    router _ = HTTPure.notFound
