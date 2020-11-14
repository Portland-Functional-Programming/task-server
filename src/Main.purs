module Main where

import Prelude

import Data.Array ((:), head, tail, filter)
import Data.Bifunctor as Bifunctor
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (split, joinWith, Pattern(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref as Ref
import Foreign.Object as Object
import HTTPure ((!!), (!@), Response)
import HTTPure as HTTPure
import HTTPure.Utils as Utils
import Node.FS.Aff as FS
import Task (Priority(..), Tag(..), Task)
import View.Tasks (renderTasks)

tasks :: Array Task
tasks = [ { name: "Buy milk"
          , priority: Medium
          , tags: [Tag "home"]
          }
        , { name: "Call Doctor"
          , priority: High
          , tags: []
          }
        ]

toPriority :: String -> Maybe Priority
toPriority "low" = Just Low
toPriority "medium" = Just Medium
toPriority "high" = Just High
toPriority _ = Nothing

serveStaticFile :: forall m. MonadAff m => String -> m Response
serveStaticFile path = liftAff $ FS.readFile path >>= HTTPure.ok

-- | Copied with slight modification from
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

createTask :: forall m. MonadAff m => Ref.Ref (Array Task) -> String -> m Response
createTask tasksRef reqBody = case maybeTask of
  Just task -> do
    liftEffect $ Ref.modify_ (task : _) tasksRef
    HTTPure.seeOther' (HTTPure.header "Location" "/tasks") ""
  Nothing -> HTTPure.badRequest "Unable to create a new task."
  where maybeTask = do
          let params = parse reqBody
          name <- params !! "name"
          priority <- params !! "priority" >>= toPriority
          pure { name: name
               , priority: priority
               , tags: []
               }

main :: HTTPure.ServerM
main = do
  tasksRef <- Ref.new tasks
  HTTPure.serve 8080 (router tasksRef) $ log "Server now up on port 8080"
  where
    router _ { path: [] } = HTTPure.permanentRedirect' (HTTPure.header "Location" "/tasks") ""
    router tasksRef { path: ["tasks"], method: HTTPure.Get } = do
      tasks' <- liftEffect $ Ref.read tasksRef
      HTTPure.ok $ renderTasks tasks'
    router tasksRef { path: ["tasks"], method: HTTPure.Post, body } = createTask tasksRef body
    router _ { path } | path !@ 0 == "static" = serveStaticFile (intercalate "/" path)
    router _ _ = HTTPure.notFound
