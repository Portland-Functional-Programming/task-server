module Model.Task where

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
instance showTag :: Show Tag where
  show (Tag tag) = tag

type Task = { id :: UUID
            , name :: String
            , priority :: Priority
            , tags :: Array Tag
            }
