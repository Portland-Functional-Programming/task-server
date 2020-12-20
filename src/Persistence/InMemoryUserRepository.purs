module Persistence.InMemoryUserRepository  where

import Prelude

import Data.Array (findIndex, head, updateAt, filter, (:))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Model.User (User, UserName)
import Effect (Effect)
import Effect.Ref (Ref, modify_)
import Effect.Ref as Ref
import Data.Maybe (Maybe(..))
import Persistence (class UserRepository)

newtype InMemoryUserRepo = InMemoryUserRepo (Ref (Array User))

instance inMemoryUserRepo :: UserRepository InMemoryUserRepo where
  getUserByUserName (InMemoryUserRepo ref) = _getUserByUserName ref
  saveUser (InMemoryUserRepo ref) = _save ref

mkInMemoryUserRepository :: Effect InMemoryUserRepo
mkInMemoryUserRepository = Ref.new [] <#> InMemoryUserRepo

_getUserByUserName :: forall m. MonadAff m => Ref (Array User) -> UserName -> m (Maybe User)
_getUserByUserName ref username = do
  users <- liftEffect $ Ref.read ref
  pure $ head $ filter (\user -> user.username == username) users

_save :: forall m. MonadAff m => Ref (Array User) -> User -> m Unit
_save ref user = do
  liftEffect $ modify_ save' ref
  where save' :: Array User -> Array User
        save' users = case findIndex (\user' -> user'.id == user.id) users of
          Just i -> case updateAt i user users of
            Just users'' -> users''
            Nothing -> users
          Nothing -> user:users
