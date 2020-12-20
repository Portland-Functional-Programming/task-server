module Persistence where

import Prelude

import Data.Maybe (Maybe)
import Data.UUID (UUID)
import Model.Task (Task)
import Effect.Aff.Class (class MonadAff)

class TaskRepository p where
  getTaskById :: forall m. MonadAff m => p -> UUID -> m (Maybe Task)
  getAll :: forall m. MonadAff m => p -> m (Array Task)
  save :: forall m. MonadAff m => p -> Task ->  m Unit
  
