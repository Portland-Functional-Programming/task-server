module Main where

import Prelude

import Debug.Trace (trace)

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
import Persistence (class Persistence, mkInMemoryPersitence)
import Persistence.UserRepository (class UserRepository, mkInMemoryUserRepository, getUserByUserName, save)
import Control.Monad.Reader.Trans (ReaderT, runReaderT, asks)
import Control.Monad.Trans.Class (lift)
import App
import Auth

serveStaticFile :: forall m. MonadAff m => String -> m Response
serveStaticFile path = liftAff $ FS.readFile path >>= HTTPure.ok

-- |Authenticatoin middleware. This is not standard HTTPure middleware as the
-- |type signature here is application specific.
-- authenticate :: forall r p. UserRepository r => Persistence p
--                => r
--                -> p
--                -> (p -> User -> Request -> ResponseM)
--                -> Request
--                -> ResponseM
-- authenticate userRepo repo f req = do
--   let maybeCreds = getCredentials req
--   case maybeCreds of
--     Just (Tuple username password) -> do
--       maybeUser <- getUserByUserName userRepo username
--       case maybeUser of
--         Just user -> auth password user
--         Nothing -> HTTPure.unauthorized
--     Nothing -> authChallenge
--   where auth password user = if user.password == password
--                     then f repo user req
--                     else HTTPure.unauthorized

authenticationMiddleware :: forall r p m. UserRepository r => Persistence p => MonadAff m
                         => (User -> Request -> AppM r p m Response)
                         -> Request
                         -> AppM r p m Response
authenticationMiddleware router req = do
  liftEffect $ log "Running authentication middleware."
  let maybeCreds = getCredentials req
  liftEffect $ log $ "maybeCreds: " <> show maybeCreds
  case maybeCreds of
    Just (Tuple username password) -> do
      userRepo <- getUserRepo
      maybeUser <- getUserByUserName userRepo username
      liftEffect $ log $ "maybeUser: " <> show maybeUser
      case maybeUser of
        Just user -> auth password user
        Nothing -> liftAff HTTPure.unauthorized
    Nothing -> liftAff authChallenge
  where auth password user = if user.password == password
                    then router user req
                    else liftAff HTTPure.unauthorized

authorizationMiddleware :: forall r p m. UserRepository r => Persistence p => MonadAff m
                         => (User -> Request -> AppM r p m Response)
                         -> User
                         -> Request
                         -> AppM r p m Response
authorizationMiddleware router loggedInUser req = do
  liftEffect $ log $ "Running authorization middleware. Path: " <> show req.path
  maybeResourceUser <- getResourceUser req
  liftEffect $ log $ "maybeResourceUser: " <> show maybeResourceUser
  case maybeResourceUser of
    Just resourceUser -> if resourceUser.username == loggedInUser.username
                         then router loggedInUser req
                         else liftAff HTTPure.unauthorized
    Nothing -> liftAff HTTPure.unauthorized

-- |Get the user whose resource is the target of this request. The assumption
-- |here is that resource path has the form `/<username>/<resource-path>`.
-- getResourceUser :: forall r m. UserRepository r => MonadAff m
--                 => r
--                 -> Request
--                 -> m (Maybe User)
-- getResourceUser _ _ = pure Nothing

getResourceUser :: forall r p m. UserRepository r => Persistence p => MonadAff m
                => Request
                -> AppM r p m (Maybe User)
getResourceUser req = do
  let userName = UserName $ req.path !@ 0
  userRepo <- getUserRepo
  liftAff $ getUserByUserName userRepo userName

-- authorize :: User
--           -> (User -> Request -> ResponseM)
--           -> Request
--           -> ResponseM
-- authorize loggedInUser router req = do
--   maybeResourceUser <- getResourceUser req
--   case maybeResourceUser of
--     Just resourceUser -> if resourceUser.username == loggedInUser.username
--                          then router loggedInUser req
--                          else HTTPure.unauthorized
--     Nothing -> HTTPure.unauthorized

-- |Routes requiring authorization

-- authRoutes :: forall p. Persistence p => p -> User -> Request -> ResponseM
-- authRoutes repo _ req@{ path: ["tasks"], method: HTTPure.Get } = TasksController.get repo req
-- authRoutes repo _ { path: ["tasks"], method: HTTPure.Post, body } = TasksController.post repo body
-- authRoutes repo _ { path: ["login"], method: HTTPure.Get } = LoginController.get
-- authRoutes repo _ req@{ path, method: HTTPure.Delete } | path !@ 0 == "task" = TaskController.delete repo req
-- authRoutes repo _ req@{ path, method: HTTPure.Patch } | path !@ 0 == "task" = TaskController.patch repo req
-- authRoutes repo _ req@{ path, method: HTTPure.Post } | path !@ 0 == "task" = TaskController.post repo req
-- authRoutes _ _ _ = HTTPure.notFound

-- |Routes requiring authorization
authRouter :: forall r p m. UserRepository r => Persistence p => MonadAff m => User -> Request -> AppM r p m Response
authRouter user req@{ path, method: HTTPure.Get } | path !@ 1 == "tasks" = let username = UserName (path !@ 0) in TasksController.get user username req
authRouter _ { path: ["tasks"], method: HTTPure.Post, body } = getTaskRepo >>= \taskRepo -> lift $ TasksController.post taskRepo body
--authRouter _ { path: ["login"], method: HTTPure.Get } = lift LoginController.get
authRouter _ req@{ path, method: HTTPure.Delete } | path !@ 0 == "task" = getTaskRepo >>= \taskRepo -> lift $ TaskController.delete taskRepo req
authRouter _ req@{ path, method: HTTPure.Patch } | path !@ 0 == "task" = getTaskRepo >>= \taskRepo -> lift $ TaskController.patch taskRepo req
authRouter _ req@{ path, method: HTTPure.Post } | path !@ 0 == "task" = getTaskRepo >>= \taskRepo -> lift $ TaskController.post taskRepo req
authRouter _ _ = HTTPure.notFound

-- router :: forall r p. UserRepository r => Persistence p => r -> p -> Request -> ResponseM
-- router _ _ req@{ path: [] } = HomeController.get req
-- router _ _ { path } | path !@ 0 == "static" = serveStaticFile (intercalate "/" path)
-- router userRepo taskRepo req = authenticate userRepo taskRepo authRoutes req

-- topLevelRouter :: forall r p. UserRepository r => Persistence p => r -> p -> (r -> p -> Request -> ResponseM) -> Request -> ResponseM
-- topLevelRouter _ _ _ req@{ path: [] } = HomeController.get req
-- topLevelRouter _ _ _ { path } | path !@ 0 == "static" = serveStaticFile (intercalate "/" path)

-- authed routes
-- topLevelRouter userRepo taskRepo authRouter req = authRouter userRepo taskRepo req

topLevelRouter' :: forall r p m. UserRepository r => Persistence p => MonadAff m => Request -> AppM r p m Response
topLevelRouter' req@{ path: [] } = lift $ HomeController.get req
topLevelRouter' req@{ path: ["login"], method: HTTPure.Get } = LoginController.get req
topLevelRouter' { path } | path !@ 0 == "static" = lift $ serveStaticFile (intercalate "/" path)
topLevelRouter' req = (authorizationMiddleware >>> authenticationMiddleware) authRouter req

router' :: forall r p. UserRepository r => Persistence p => Env r p -> Request -> ResponseM
router' env req = liftAff $ runApp (topLevelRouter' req) env

-- f >>> g :: (a -> b) -> (b -> c) -> a -> c
--
-- type Env = Tuple UserRepository TaskRepository
--
-- Reader Env ResponseM
--
-- type App = Reader Env ResponseM
--
-- router :: r -> p -> User -> Request -> ResponseM
-- router' :: User -> App
-- router'' :: Request -> App
--
-- Ensures that the request is authenticated. Then gets the authenticated user and passes it on to the passed in router.
-- authenticationMiddleware :: r -> p -> (r -> p -> User -> Request -> ResponseM) -> Request -> ResponseM
-- authenticationMiddleware' :: (User -> App) -> Router
-- authenticationMiddleware'' :: (User -> Request -> App) -> Request -> App
--
-- :type ((authenticationMiddleware userRepo taskRepo) authRouter) :: Request -> ResponseM
--
-- 
-- authorizationMiddleware :: r -> p -> (r -> p -> User -> Request -> ResponseM) -> Request -> ResponseM
-- authorizationMiddleware' :: (User -> Request -> App) -> User -> Request -> App
--
-- :type (authenticationMiddleware userRepo taskRepo) >>> (authorizationMiddleware userRepo taskRepo) :: Request -> ResponseM
--
-- UserRepository -> Persistence -> 

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
