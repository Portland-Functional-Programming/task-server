module Controller.Tasks (get, post) where

import Prelude

import Data.Array (head, tail, filter)
import Data.Bifunctor as Bifunctor
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (split, joinWith, Pattern(..), contains)
import Data.Tuple (Tuple(..))
import Data.UUID (UUID, genUUID)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Foreign.Object as Object
import HTTPure (Request, Response, (!!), lookup)
import HTTPure as HTTPure
import HTTPure.Utils as Utils
import View.HTML.Tasks as HTML
import View.JSON.Tasks as JSON
import Model.Task (Task, Priority(..), create, Status(Deleted))
import Persistence (class Persistence, getAll, save)

data AcceptType = HTML
                | JSON
                | Other

toPriority :: String -> Maybe Priority
toPriority "low" = Just Low
toPriority "medium" = Just Medium
toPriority "high" = Just High
toPriority _ = Nothing

-- | Copied with modification from
-- https://github.com/cprussin/purescript-httpure/blob/a81abca2d64bd9805874c4a2a80c07144fd19d11/src/HTTPure/Query.purs#L29
parse :: String -> HTTPure.Query
parse = split' "&" >>> nonempty >>> toObject
  where
    toObject = map toTuple >>> Object.fromFoldable
    nonempty = filter ((/=) "")
    split' = Pattern >>> split
    first = head >>> fromMaybe ""
    last = tail >>> fromMaybe [] >>> joinWith ""
    decode = Utils.replacePlus >>> Utils.urlDecode
    decodeKeyValue = Bifunctor.bimap decode decode
    toTuple item = decodeKeyValue $ Tuple (first itemParts) (last itemParts)
      where
        itemParts = split' "=" item

wantsJSON :: Request -> Boolean
wantsJSON { headers } = case lookup headers "Accept" of
  Just accept -> contains (Pattern "application/json") accept
  Nothing -> false

acceptTypeFromRequest :: Request -> AcceptType
acceptTypeFromRequest req = if wantsJSON req then JSON else HTML

get :: forall m p. MonadAff m => Persistence p => p -> Request -> m Response
get repo req = do
  tasks' <- getAll repo
  let undeletedTasks = filter (\task -> task.status /= Deleted) tasks'
  case acceptTypeFromRequest req of
    HTML -> HTTPure.ok $ HTML.render undeletedTasks
    JSON -> HTTPure.ok $ JSON.render undeletedTasks
    Other -> HTTPure.notAcceptable

post :: forall m p. MonadAff m => Persistence p => p -> String -> m Response
post repo reqBody = do
  uuid <- liftEffect genUUID
  case createTask uuid of
    Just task -> do
      save repo task
      HTTPure.seeOther' (HTTPure.header "Location" "/tasks") ""
    Nothing -> HTTPure.badRequest "Unable to create a new task."
  where createTask :: UUID -> Maybe Task
        createTask uuid = do
          let params = parse reqBody
          name <- params !! "name"
          priority <- params !! "priority" >>= toPriority
          pure $ create uuid name priority []

