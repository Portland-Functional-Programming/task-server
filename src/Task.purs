module Task where

import Prelude

data Priority = High
              | Medium
              | Low
instance showPriority :: Show Priority where
  show High = "high"
  show Medium = "medium"
  show Low = "low"

data Tag = Tag String

type Task = { name :: String
            , priority :: Priority
            , tags :: Array Tag
            }
