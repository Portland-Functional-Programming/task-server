module Controller.Task (delete) where

import Prelude

import Data.Array (findIndex, deleteAt)
import Data.Maybe (Maybe(..))
import Data.UUID (parseUUID, UUID)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import HTTPure (Request, Response, (!@))
import HTTPure as HTTPure
import Model.Task (Task)

deleteTask :: UUID -> Array Task -> Array Task
deleteTask uuid tasks = case maybeTasks of
  Just tasks' -> tasks'
  Nothing -> tasks
  where maybeTasks = do
          i <- findIndex (\{ id } -> id == uuid) tasks
          deleteAt i tasks

delete :: forall m. MonadAff m => Ref.Ref (Array Task) -> Request -> m Response
delete tasksRef { path } = case maybeUUID of
  Just uuid -> do
    liftEffect $ Ref.modify_ (deleteTask uuid) tasksRef
    HTTPure.seeOther' (HTTPure.header "Location" "/tasks") ""
  Nothing -> HTTPure.badRequest "Invalid task ID."
  where maybeUUID = parseUUID (path !@ 1)
