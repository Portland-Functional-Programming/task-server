module Model.User where

import Prelude

newtype UserId = UserId String
derive instance userIdEq :: Eq UserId
instance userIdShow :: Show UserId where
  show (UserId userId) = userId
newtype UserName = UserName String
derive instance userNameEq :: Eq UserName
instance userNameShow :: Show UserName where
  show (UserName username) = username

type User =
  { id :: UserId
  , username :: UserName
  , password :: String
  }
