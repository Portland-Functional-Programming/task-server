module Main where

import Prelude

import Data.Foldable (intercalate)
import Data.Maybe (fromJust)
import Data.UUID (parseUUID)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Console (log)
import Effect.Ref as Ref
import HTTPure ((!@), Response)
import HTTPure as HTTPure
import Node.FS.Aff as FS
import Model.Task (Priority(..), Tag(..), Task, Status(..))
import Controller.Tasks as TasksController
import Controller.Task as TaskController
import Partial.Unsafe (unsafePartial)

tasks :: Array Task
tasks = [ { id: unsafePartial $ fromJust $ parseUUID "35fb5c26-6478-4756-9894-a1225a5cd838"
          , name: "Buy milk"
          , status: Backlog
          , priority: Medium
          , tags: [Tag "home"]
          }
        , { id: unsafePartial $ fromJust $ parseUUID "b59f3bb1-61f3-4d1b-b47c-83f340446a01"
          , name: "Call Doctor"
          , status: DueToday
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
    router tasksRef req@{ path: ["tasks"], method: HTTPure.Get } = TasksController.get tasksRef req
    router tasksRef { path: ["tasks"], method: HTTPure.Post, body } = TasksController.post tasksRef body
    router tasksRef req@{ path, method: HTTPure.Delete } | path !@ 0 == "task" = TaskController.delete tasksRef req
    router _ { path } | path !@ 0 == "static" = serveStaticFile (intercalate "/" path)
    router _ _ = HTTPure.notFound
