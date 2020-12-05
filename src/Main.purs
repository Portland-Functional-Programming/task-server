module Main where

import Prelude

import Debug.Trace (trace)

import Effect.Console (log)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Data.String (stripPrefix, split, Pattern(..))
import Data.String.Base64 (decode)
import Effect.Aff (launchAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import HTTP.Query (parse)
import HTTPure ((!!), (!@), Request, Response, ResponseM)
import HTTPure as HTTPure
import Node.FS.Aff as FS
import Control.Error.Util (hush)
import Controller.Tasks as TasksController
import Controller.Task as TaskController
import Controller.Home as HomeController
import Controller.Login as LoginController
import Model.User
import Persistence (class Persistence, mkInMemoryPersitence)
import Persistence.UserRepository (class UserRepository, mkInMemoryUserRepository, getUserByUserName, save)

serveStaticFile :: forall m. MonadAff m => String -> m Response
serveStaticFile path = liftAff $ FS.readFile path >>= HTTPure.ok

authChallenge :: HTTPure.ResponseM
authChallenge = HTTPure.unauthorized' $ HTTPure.header "WWW-Authenticate" "Basic realm=\"Task Scheduler\""

getCreds :: Request -> Maybe (Tuple UserName String)
getCreds req = do
  credsArray <- req.headers !! "authorization" >>= (stripPrefix (Pattern "Basic ") >=> (decode >>> hush)) <#> split (Pattern ":")
  case credsArray of
    [username, password] -> Just (Tuple (UserName username) password)
    _ -> Nothing

authenticate :: forall r p. UserRepository r => Persistence p
               => r
               -> p
               -> (p -> User -> Request -> ResponseM)
               -> Request
               -> ResponseM
authenticate userRepo repo f req = do
  let maybeCreds = getCreds req
  case maybeCreds of
    Just (Tuple username password) -> do
      maybeUser <- liftAff $ getUserByUserName userRepo username
      case maybeUser of
        Just user -> auth password user
        Nothing -> HTTPure.unauthorized
    Nothing -> authChallenge
  where auth password user = if user.password == password
                    then f repo user req
                    else HTTPure.unauthorized

-- |Routes requiring authorization
authRoutes :: forall p. Persistence p => p -> User -> Request -> ResponseM
authRoutes repo _ req@{ path: ["tasks"], method: HTTPure.Get } = TasksController.get repo req
authRoutes repo _ { path: ["tasks"], method: HTTPure.Post, body } = TasksController.post repo body
authRoutes repo _ { path: ["login"], method: HTTPure.Get } = LoginController.get
authRoutes repo _ req@{ path, method: HTTPure.Delete } | path !@ 0 == "task" = TaskController.delete repo req
authRoutes repo _ req@{ path, method: HTTPure.Patch } | path !@ 0 == "task" = TaskController.patch repo req
authRoutes repo _ req@{ path, method: HTTPure.Post } | path !@ 0 == "task" = TaskController.post repo req
authRoutes _ _ _ = HTTPure.notFound

router :: forall r p. UserRepository r => Persistence p => r -> p -> Request -> ResponseM
router _ _ req@{ path: [] } = HomeController.get req
router _ _ { path } | path !@ 0 == "static" = serveStaticFile (intercalate "/" path)
router userRepo taskRepo req = authenticate userRepo taskRepo authRoutes req

main :: HTTPure.ServerM
main = do
  taskRepo <- mkInMemoryPersitence
  userRepo <- mkInMemoryUserRepository
  let user = { id: (UserId "12345")
             , username: (UserName "tim")
             , password: "password"
             }
  launchAff_ $ save userRepo user
  HTTPure.serve 8080 (router userRepo taskRepo) $ log "Server now up on port 8080"
