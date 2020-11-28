module Main where

import Prelude

import Data.Foldable (intercalate)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Console (log)
import HTTPure ((!@), Response)
import HTTPure as HTTPure
import Node.FS.Aff as FS
import Controller.Tasks as TasksController
import Controller.Task as TaskController
import Persistence (mkInMemoryPersitence)

serveStaticFile :: forall m. MonadAff m => String -> m Response
serveStaticFile path = liftAff $ FS.readFile path >>= HTTPure.ok

main :: HTTPure.ServerM
main = do
  repo <- mkInMemoryPersitence
  HTTPure.serve 8080 (router repo) $ log "Server now up on port 8080"
  where
    router _ { path: [] } = HTTPure.permanentRedirect' (HTTPure.header "Location" "/tasks") ""
    router repo req@{ path: ["tasks"], method: HTTPure.Get } = TasksController.get repo req
    router repo { path: ["tasks"], method: HTTPure.Post, body } = TasksController.post repo body
    router repo req@{ path, method: HTTPure.Delete } | path !@ 0 == "task" = TaskController.delete repo req
    router repo req@{ path, method: HTTPure.Patch } | path !@ 0 == "task" = TaskController.patch repo req
    router repo req@{ path, method: HTTPure.Post } | path !@ 0 == "task" = TaskController.post repo req
    router _ { path } | path !@ 0 == "static" = serveStaticFile (intercalate "/" path)
    router _ _ = HTTPure.notFound
