module Main where

import Prelude

import Controller.Task as TaskController
import Data.Foldable (intercalate)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref as Ref
import HTTPure ((!@), Response)
import HTTPure as HTTPure
import Node.FS.Aff as FS
import Task (Priority(..), Tag(..), Task)
import View.Tasks (renderTasks)

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
main = do
  tasksRef <- Ref.new tasks
  HTTPure.serve 8080 (router tasksRef) $ log "Server now up on port 8080"
  where
    router _ { path: [] } = HTTPure.permanentRedirect' (HTTPure.header "Location" "/tasks") ""
    router tasksRef { path: ["tasks"], method: HTTPure.Get } = do
      tasks' <- liftEffect $ Ref.read tasksRef
      HTTPure.ok $ renderTasks tasks'
    router tasksRef { path: ["tasks"], method: HTTPure.Post, body } = TaskController.post tasksRef body
    router _ { path } | path !@ 0 == "static" = serveStaticFile (intercalate "/" path)
    router _ _ = HTTPure.notFound
