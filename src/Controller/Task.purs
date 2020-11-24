module Controller.Task (post, delete) where

import Prelude

import Data.Array (findIndex, deleteAt, filter, head, tail)
import Data.Bifunctor as Bifunctor
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (split, joinWith, Pattern(..))
import Data.Tuple (Tuple(..))
import Data.UUID (parseUUID, UUID, toString)
import Effect.Aff.Class (class MonadAff)
import Foreign.Object as Object
import HTTPure (Request, Response, (!@), (!!))
import HTTPure as HTTPure
import HTTPure.Utils as Utils
import Model.Task (Task, Status(Deleted))
import Persistence (class Persistence, getTaskById, save)

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

post :: forall m p. MonadAff m => Persistence p => p -> Request -> m Response
post repo req = do
  let params = parse req.body
  case params !! "_method" of
    Just "delete" -> delete repo req
    _ -> HTTPure.badRequest "POSTing to a task URI is currently only used for deleting a task."

deleteTask :: UUID -> Array Task -> Array Task
deleteTask uuid tasks = case maybeTasks of
  Just tasks' -> tasks'
  Nothing -> tasks
  where maybeTasks = do
          i <- findIndex (\{ id } -> id == uuid) tasks
          deleteAt i tasks

delete :: forall m p. MonadAff m => Persistence p => p -> Request -> m Response
delete repo { path } = case maybeUUID of
  Just uuid -> do
    maybeTask <- getTaskById repo uuid
    case maybeTask of
      Just task -> do
        save repo (task { status = Deleted })
        HTTPure.seeOther' (HTTPure.header "Location" "/tasks") ""
      Nothing -> HTTPure.badRequest ("Could not delete with ID " <> toString uuid <> ".")
  Nothing -> HTTPure.badRequest "Invalid task ID."
  where maybeUUID = parseUUID (path !@ 1)
