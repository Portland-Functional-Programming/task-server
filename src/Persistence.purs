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

class TaskRepository p where
  getTaskById :: forall m. MonadAff m => p -> UUID -> m (Maybe Task)
  getAll :: forall m. MonadAff m => p -> m (Array Task)
  save :: forall m. MonadAff m => p -> Task ->  m Unit

newtype InMemoryTaskRepository = InMemoryTaskRepository (Ref (Array Task))

instance inMemoryTaskRepository :: TaskRepository InMemoryTaskRepository where
  getTaskById (InMemoryTaskRepository ref) = _getTaskById ref
  getAll (InMemoryTaskRepository ref) = _getAll ref
  save (InMemoryTaskRepository ref) = _save ref

mkInMemoryPersitence :: Effect InMemoryTaskRepository
mkInMemoryPersitence = Ref.new [] <#> InMemoryTaskRepository

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
