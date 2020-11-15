module Task where

import Prelude
import Data.UUID (UUID)

data Priority = High
              | Medium
              | Low
instance showPriority :: Show Priority where
  show High = "high"
  show Medium = "medium"
  show Low = "low"

data Tag = Tag String

type Task = { id :: UUID
            , name :: String
            , priority :: Priority
            , tags :: Array Tag
            }
