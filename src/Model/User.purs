module Model.User where

import Prelude

newtype UserId = UserId String
derive instance userIdEq :: Eq UserId
newtype UserName = UserName String
derive instance userNameEq :: Eq UserName

type User =
  { id :: UserId
  , username :: UserName
  , password :: String
  }
