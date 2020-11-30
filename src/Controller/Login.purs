module Controller.Login (get) where

import Prelude

import HTTPure as HTTPure
import HTTPure (Request, Response, (!!), lookup)
import Effect.Aff.Class (class MonadAff)
import HTTP.Query (parse)
import Data.Maybe (maybe)

get :: forall m. MonadAff m => m Response
get = HTTPure.unauthorized' $ HTTPure.header "WWW-Authenticate" "Basic realm=\"Task Scheduler\""

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
