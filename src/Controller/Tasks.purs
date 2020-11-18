module Controller.Tasks (get) where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Effect.Ref (Ref)
import HTTPure (Request, Response)
import HTTPure as HTTPure
import View.HTML.Tasks as HTML
import View.JSON.Tasks as JSON
import Model.Task (Task)

data AcceptType = HTML
                | JSON
                | Other

acceptTypeFromRequest :: Request -> AcceptType
acceptTypeFromRequest _ = JSON

get :: forall m. MonadAff m => Ref (Array Task) -> Request -> m Response
get tasksRef req = do
  tasks' <- liftEffect $ Ref.read tasksRef
  case acceptTypeFromRequest req of
    HTML -> HTTPure.ok $ HTML.render tasks'
    JSON -> HTTPure.ok $ JSON.render tasks'
    Other -> HTTPure.notAcceptable

