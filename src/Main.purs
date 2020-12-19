module Main where

import Prelude

import Effect.Console (log)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..), fst, snd)
import Data.String (stripPrefix, split, Pattern(..))
import Effect.Aff (launchAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import HTTP.Query (parse)
import HTTPure ((!!), (!@), Request, Response, ResponseM)
import HTTPure as HTTPure
import Node.FS.Aff as FS
import Controller.Tasks as TasksController
import Controller.Task as TaskController
import Controller.Home as HomeController
import Controller.Login as LoginController
import Model.User
import Persistence (class TaskRepository, mkInMemoryPersitence)
import Persistence.UserRepository (class UserRepository, mkInMemoryUserRepository, getUserByUserName, save)
import Control.Monad.Reader.Trans (ReaderT, runReaderT, asks)
import Control.Monad.Trans.Class (lift)
import App
import Auth

serveStaticFile :: forall m. MonadAff m => String -> m Response
serveStaticFile path = liftAff $ FS.readFile path >>= HTTPure.ok

authenticationMiddleware :: forall r p m. UserRepository r => TaskRepository p => MonadAff m
                         => (User -> Request -> AppM r p m Response)
                         -> Request
                         -> AppM r p m Response
authenticationMiddleware router req = do
  let maybeCreds = getCredentials req
  case maybeCreds of
    Just (Tuple username password) -> do
      userRepo <- getUserRepo
      maybeUser <- getUserByUserName userRepo username
      case maybeUser of
        Just user -> auth password user
        Nothing -> liftAff HTTPure.unauthorized
    Nothing -> liftAff authChallenge
  where auth password user = if user.password == password
                    then router user req
                    else liftAff HTTPure.unauthorized

authorizationMiddleware :: forall r p m. UserRepository r => TaskRepository p => MonadAff m
                         => (User -> Request -> AppM r p m Response)
                         -> User
                         -> Request
                         -> AppM r p m Response
authorizationMiddleware router loggedInUser req = do
  maybeResourceUser <- getResourceUser req
  case maybeResourceUser of
    Just resourceUser -> if resourceUser.username == loggedInUser.username
                         then router loggedInUser req
                         else liftAff HTTPure.unauthorized
    Nothing -> liftAff HTTPure.unauthorized

getResourceUser :: forall r p m. UserRepository r => TaskRepository p => MonadAff m
                => Request
                -> AppM r p m (Maybe User)
getResourceUser req = do
  let userName = UserName $ req.path !@ 0
  userRepo <- getUserRepo
  liftAff $ getUserByUserName userRepo userName

-- |Routes requiring authorization
authRouter :: forall r p m. UserRepository r => TaskRepository p => MonadAff m => User -> Request -> AppM r p m Response
authRouter user req@{ path, method: HTTPure.Get } | path !@ 1 == "tasks" = let username = UserName (path !@ 0) in TasksController.get user username req
authRouter user req@{ path: ["tasks"], method: HTTPure.Post } = TasksController.post user req
authRouter _ req@{ path, method: HTTPure.Delete } | path !@ 0 == "task" = getTaskRepo >>= \taskRepo -> lift $ TaskController.delete taskRepo req
authRouter _ req@{ path, method: HTTPure.Patch } | path !@ 0 == "task" = getTaskRepo >>= \taskRepo -> lift $ TaskController.patch taskRepo req
authRouter _ req@{ path, method: HTTPure.Post } | path !@ 0 == "task" = getTaskRepo >>= \taskRepo -> lift $ TaskController.post taskRepo req
authRouter _ _ = HTTPure.notFound

topLevelRouter' :: forall r p m. UserRepository r => TaskRepository p => MonadAff m => Request -> AppM r p m Response
topLevelRouter' req@{ path: [] } = lift $ HomeController.get req
topLevelRouter' req@{ path: ["login"], method: HTTPure.Get } = LoginController.get req
topLevelRouter' { path } | path !@ 0 == "static" = lift $ serveStaticFile (intercalate "/" path)
topLevelRouter' req = authenticationMiddleware authRouter req

router' :: forall r p. UserRepository r => TaskRepository p => Env r p -> Request -> ResponseM
router' env req = liftAff $ runApp (topLevelRouter' req) env

main :: HTTPure.ServerM
main = do
  taskRepo <- mkInMemoryPersitence
  userRepo <- mkInMemoryUserRepository
  let env = Tuple userRepo taskRepo
      user = { id: (UserId "12345")
             , username: (UserName "tim")
             , password: "password"
             }
  launchAff_ $ save userRepo user
  HTTPure.serve 8080 (router' env) $ log "Server now up on port 8080"
