module Quill.API.Delta
    ( Delta(..)
    , readDelta
    ) where

import Prelude

import Control.Monad.Error.Class (throwError)

import Data.Foreign (F, Foreign, ForeignError(..), readString)
import Data.Foreign.Index ((!))
import Data.Foreign.Keys (keys)
import Data.List.NonEmpty (singleton)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

import Quill.API.FormatSpec (FormatSpec, readFormatSpec)

-- | https://quilljs.com/docs/delta/
data Delta
    = InsertString String (Array FormatSpec)
    | DeleteString String (Array FormatSpec)
    | RetainString String (Array FormatSpec)
    -- TODO: embeds

-- | Attempt to read in a Delta from a `Foreign` value.
readDelta :: Foreign -> F Delta
readDelta value = do
    keys' <- keys value
    case keys' of
        [ "insert" ] -> do
            str <- value ! "insert" >>= readString
            pure $ InsertString str []

        [ "insert", "attributes" ] -> do
            str   <- value ! "insert" >>= readString
            attrs <- value ! "attributes"
            fmts  <- keys attrs >>= traverse \(k :: String) -> do
                        readFormatSpec =<< (attrs ! k <#> Tuple k)
            pure $ InsertString str fmts

        -- TODO: finish this!

        _ -> do
            throwError $ (singleton
                 (ForeignError $ "unrecognised Delta properties: " <> show keys'))

