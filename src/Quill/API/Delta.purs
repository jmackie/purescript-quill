module Quill.API.Delta
    ( Deltas
    , readDeltas
    , Delta(..)
    , readDelta
    ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)

import Data.Either (Either(..))
import Data.Foreign (F, Foreign, ForeignError(..),
                     readString, readInt, readArray)
import Data.Foreign.Index ((!))
import Data.Foreign.Keys (keys)
import Data.List.NonEmpty (singleton)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

import Quill.API.Embed (Embed(..))
import Quill.API.FormatSpec (FormatSpec, readFormatSpec)

type Deltas = Array Delta

readDeltas :: Foreign -> F Deltas
readDeltas = readArray >=> traverse readDelta

-- | https://quilljs.com/docs/delta/
data Delta
    = Insert (Either Embed String) (Array FormatSpec)
    | Delete Int
    | Retain Int (Array FormatSpec)

-- | Attempt to read in a Delta from a `Foreign` value.
readDelta :: Foreign -> F Delta
readDelta f = do
    keys' <- keys f
    case keys' of
        [ "insert" ] -> do
            value <- f ! "insert" >>= \v ->
                        (readString v <#> Right)
                    <|> (v ! "image" >>= readString <#> Left <<< Image)
                    <|> (v ! "Video" >>= readString <#> Left <<< Video)
            pure $ Insert value []

        [ "insert", "attributes" ] -> do
            value <- f ! "insert" >>= \v ->
                        (readString v <#> Right)
                    <|> (v ! "image" >>= readString <#> Left <<< Image)
                    <|> (v ! "Video" >>= readString <#> Left <<< Video)

            attrs <- f ! "attributes"
            fmts  <- keys attrs >>= traverse \(k :: String) -> do
                        readFormatSpec =<< (attrs ! k <#> Tuple k)

            pure $ Insert value fmts

        [ "delete" ] -> do
            f ! "delete" >>= readInt <#> Delete

        [ "retain" ] -> do
            f ! "retain" >>= readInt <#> flip Retain []

        [ "retain", "attributes" ] -> do
            value <- f ! "retain" >>= readInt

            attrs <- f ! "attributes"
            fmts  <- keys attrs >>= traverse \(k :: String) -> do
                        readFormatSpec =<< (attrs ! k <#> Tuple k)

            pure $ Retain value fmts

        _ ->
            throwError $ (singleton
                 (ForeignError $ "unrecognised Delta properties: " <> show keys'))

