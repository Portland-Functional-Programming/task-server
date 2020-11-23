module Persistence where

import Prelude

import Data.Array (head, filter, findIndex, updateAt, (:))
import Data.Maybe (Maybe(..), fromJust)
import Effect.Ref (Ref, modify_)
import Effect.Ref as Ref
import Data.UUID (UUID, parseUUID)
import Model.Task (Priority(..), Tag(..), Task, Status(..))
import Effect.Aff.Class (class MonadAff)
import Effect (Effect)
import Effect.Class (liftEffect)
import Partial.Unsafe (unsafePartial)

type Persistence m = { getTaskById :: UUID -> m (Maybe Task)
                     , getAll :: m (Array Task)
                     , save :: Task ->  m Unit
                     }

mkInMemoryPersitence :: forall m. MonadAff m => Effect (Persistence m)
mkInMemoryPersitence = do
  ref <- Ref.new []
  let repo = { getTaskById: _getTaskById ref
             , getAll: _getAll ref
             , save: _save ref
             }
  pure repo

_getTaskById :: forall m. MonadAff m => Ref (Array Task) -> UUID -> m (Maybe Task)
_getTaskById ref uuid = do
  tasks <- liftEffect $ Ref.read ref
  pure $ head $ filter (\task -> task.id == uuid) tasks

_getAll :: forall m. MonadAff m => Ref (Array Task) -> m (Array Task)
_getAll = liftEffect <<< Ref.read

_save :: forall m. MonadAff m => Ref (Array Task) -> Task -> m Unit
_save ref task = do
  liftEffect $ modify_ save' ref
  where save' :: Array Task -> Array Task
        save' tasks = case findIndex (\task' -> task'.id == task.id) tasks of
          Just i -> case updateAt i task tasks of
            Just tasks'' -> tasks''
            Nothing -> tasks
          Nothing -> task:tasks
  
tasks' :: Array Task
tasks' = [ { id: unsafePartial $ fromJust $ parseUUID "35fb5c26-6478-4756-9894-a1225a5cd838"
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
