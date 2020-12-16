module Controller.Login (get) where

import Prelude

import HTTPure as HTTPure
import HTTPure (Request, Response, (!!), lookup)
import Effect.Aff.Class (class MonadAff, liftAff)
import HTTP.Query (parse)
import Data.Maybe (maybe)
import Persistence.UserRepository (class UserRepository, getUserByUserName)
import Effect.Class (liftEffect)
import Effect.Console (log)
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
      liftEffect $ log $ "maybeUser: " <> show maybeUser
      case maybeUser of
        Just { username: UserName userName, password: realPassword } ->
          if password == realPassword
          then liftAff $ HTTPure.seeOther' (HTTPure.header "Location" ("/" <> userName <> "/tasks")) ""
          else liftAff HTTPure.unauthorized
        Nothing -> liftAff HTTPure.unauthorized
    Nothing -> liftAff authChallenge

-- post :: forall m r. MonadAff m => UserRepository r => r -> String -> m Response
-- post repo body = maybe HTTPure.unauthorized login maybeCreds
--   where
--     maybeCreds :: Maybe (UserName, String)
--     maybeCreds = do
--       let params = parse body
--       username <- params !! "username"
--       password <- params !! "password"
--       pure (username, password)

--     login :: (UserName, String) -> 
