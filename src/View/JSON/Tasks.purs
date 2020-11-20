module View.JSON.Tasks (render) where

import Prelude

import Data.UUID (toString)
import Model.Task (Task)
import Simple.JSON as JSON

type JSONTask =
  { id :: String
  , name :: String
  , priority :: String
  , tags :: Array String
  }

taskToJsonTask :: Task -> JSONTask
taskToJsonTask task =
  { id: toString task.id
  , name: task.name
  , priority: show task.priority
  , tags: map show task.tags
  }

render :: Array Task -> String
render tasks = JSON.writeJSON (map taskToJsonTask tasks)
