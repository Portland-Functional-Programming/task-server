module App where

import Control.Monad.Reader.Trans (ReaderT, runReaderT, asks)
import Effect.Aff.Class (class MonadAff)
import Persistence.UserRepository (class UserRepository)
import Persistence (class Persistence)
import Data.Tuple (Tuple, fst, snd)

type Env u t = Tuple u t
type AppM u t m a = ReaderT (Env u t) m a
runApp :: forall r p m a. UserRepository r => Persistence p => MonadAff m => AppM r p m a -> Env r p -> m a
runApp app env = runReaderT app env

getUserRepo :: forall u t m. UserRepository u => Persistence t => MonadAff m => AppM u t m u
getUserRepo = asks fst

getTaskRepo :: forall u t m. UserRepository u => Persistence t => MonadAff m => AppM u t m t
getTaskRepo = asks snd
