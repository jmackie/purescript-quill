module Quill.API.Delta
    ( Ops
    , readOps
    , Delta(..)
    , readDelta
    ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)

import Data.Array as A
import Data.Either (Either(..))
import Data.Foreign (F, Foreign, ForeignError(..), readString, readInt, readArray)
import Data.Foreign.Index ((!))
import Data.Foreign.Keys (keys)
import Data.List.NonEmpty (singleton)
import Data.Options (Options(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

import Quill.API.Embed (Embed(..))
import Quill.API.Formats (Formats)

type Ops = Array Delta

readOps :: Foreign -> F Ops
readOps = (_ ! "ops") >=> readArray >=> traverse readDelta

-- | https://quilljs.com/docs/delta/
data Delta
    = Insert (Either Embed String) (Options Formats)
    | Delete Int
    | Retain Int (Options Formats)

-- | Attempt to read in a Delta from a `Foreign` value.
readDelta :: Foreign -> F Delta
readDelta f = do
    keys' <- A.sortBy (flip compare) <$> keys f
    case keys' of
        [ "insert" ] -> do
            value <- f ! "insert" >>= \v ->
                        (readString v <#> Right)
                    <|> (v ! "image" >>= readString <#> Left <<< Image)
                    <|> (v ! "Video" >>= readString <#> Left <<< Video)
            pure $ Insert value (Options [])

        [ "insert", "attributes" ] -> do
            value <- f ! "insert" >>= \v ->
                        (readString v <#> Right)
                    <|> (v ! "image" >>= readString <#> Left <<< Image)
                    <|> (v ! "Video" >>= readString <#> Left <<< Video)

            attrs <- f ! "attributes"
            fmts  <- Options <$> (
                keys attrs >>= traverse \k -> Tuple k <$> attrs ! k)

            pure $ Insert value fmts

        [ "delete" ] -> do
            f ! "delete" >>= readInt <#> Delete

        [ "retain" ] -> do
            f ! "retain" >>= readInt <#> flip Retain (Options [])

        [ "retain", "attributes" ] -> do
            value <- f ! "retain" >>= readInt

            attrs <- f ! "attributes"
            fmts  <- Options <$> (
                keys attrs >>= traverse \k -> Tuple k <$> attrs ! k)

            pure $ Retain value fmts

        _ ->
            throwError $ (singleton
                 (ForeignError $ "unrecognised Delta properties: " <> show keys'))

