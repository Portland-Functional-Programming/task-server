module Model.Task where

import Prelude
import Data.UUID (UUID)
import Model.User (User)

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

data Status = Backlog
            | DueToday
            | Done
            | Deleted
derive instance statusEq :: Eq Status
instance showStatus :: Show Status where
  show Backlog = "Backlog"
  show DueToday = "Due today"
  show Done = "Done"
  show Deleted = "Deleted"

type Task = { id :: UUID
            , name :: String
            , status :: Status
            , priority :: Priority
            , tags :: Array Tag
            , createdBy :: User
            }

create :: UUID
       -> String
       -> Priority
       -> Array Tag
       -> User
       -> Task
create id name priority tags user =
  { id, name, status: Backlog, priority, tags, createdBy: user }
