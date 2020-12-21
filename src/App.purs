module App where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Control.Monad.Reader.Trans (ReaderT, runReaderT, asks)
import Control.Monad.Trans.Class (class MonadTrans)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Persistence (class TaskRepository, class UserRepository)
import Data.Tuple (Tuple, fst, snd)

type Env u t = Tuple u t
newtype AppM u t m a = AppM (ReaderT (Env u t) m a)

derive newtype instance functorAppM :: Monad m => Functor (AppM u t m)
derive newtype instance applyAppM :: Apply m => Apply (AppM u t m)
derive newtype instance applicativeAppM :: Applicative m => Applicative (AppM u t m)
derive newtype instance bindAppM :: Bind m => Bind (AppM u t m)
derive newtype instance monadAppM :: Monad m => Monad (AppM u t m)
derive newtype instance monadAskAppM :: Monad m => MonadAsk (Tuple u t) (AppM u t m)
derive newtype instance monadAffAppM :: MonadAff m => MonadAff (AppM u t m)
derive newtype instance monadEffectAppM :: MonadEffect m => MonadEffect (AppM u t m)
derive newtype instance monadTransAppM :: MonadTrans (AppM u t)

runApp :: forall r p m a. UserRepository r => TaskRepository p => MonadAff m => AppM r p m a -> Env r p -> m a
runApp (AppM reader) env = runReaderT reader env

getUserRepo :: forall u t m. UserRepository u => TaskRepository t => MonadAff m => AppM u t m u
getUserRepo = asks fst

getTaskRepo :: forall u t m. UserRepository u => TaskRepository t => MonadAff m => AppM u t m t
getTaskRepo = asks snd
