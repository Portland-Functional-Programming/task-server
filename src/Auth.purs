module Auth where

import Prelude

import HTTPure as HTTPure
import HTTPure (Request, (!!))
import Data.String (stripPrefix, split, Pattern(..))
import Data.String.Base64 (decode)
import Control.Error.Util (hush)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Model.User

getCredentials :: Request -> Maybe (Tuple UserName String)
getCredentials req = do
  credsArray <- req.headers !! "authorization" >>= (stripPrefix (Pattern "Basic ") >=> (decode >>> hush)) <#> split (Pattern ":")
  case credsArray of
    [username, password] -> Just (Tuple (UserName username) password)
    _ -> Nothing

authChallenge :: HTTPure.ResponseM
authChallenge = HTTPure.unauthorized' $ HTTPure.header "WWW-Authenticate" "Basic realm=\"Task Scheduler\""
