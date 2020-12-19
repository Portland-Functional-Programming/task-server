module Controller.Login (get) where

import Prelude

import HTTPure as HTTPure
import HTTPure (Request, Response, (!!), lookup)
import Effect.Aff.Class (class MonadAff, liftAff)
import HTTP.Query (parse)
import Data.Maybe (maybe)
import Persistence.UserRepository (class UserRepository, getUserByUserName)
import Effect.Class (liftEffect)
import Persistence (class Persistence)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import App
import Auth
import Model.User (UserName(..))

get :: forall r p m. UserRepository r => Persistence p => MonadAff m
    => Request
    -> AppM r p m Response
get req = do
  let maybeCreds = getCredentials req
  case maybeCreds of
    Just (Tuple username password) -> do
      userRepo <- getUserRepo
      maybeUser <- getUserByUserName userRepo username
      case maybeUser of
        Just { username: UserName userName, password: realPassword } ->
          if password == realPassword
          then liftAff $ HTTPure.seeOther' (HTTPure.header "Location" ("/" <> userName <> "/tasks")) ""
          else liftAff HTTPure.unauthorized
        Nothing -> liftAff HTTPure.unauthorized
    Nothing -> liftAff authChallenge
