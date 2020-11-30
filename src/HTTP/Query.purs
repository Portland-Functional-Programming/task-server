module HTTP.Query (parse) where

import Prelude

import HTTPure as HTTPure
import HTTPure.Utils as Utils
import Data.Tuple (Tuple(..))
import Data.String (split, joinWith, Pattern(..), contains)
import Data.Bifunctor as Bifunctor
import Foreign.Object as Object
import Data.Array (head, tail, filter)
import Data.Maybe (fromMaybe)

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
