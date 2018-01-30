module Quill.API.Delta
    ( Ops
    , readOps
    , opsToForeign
    , Delta(..)
    , readDelta
    , deltaToForeign
    ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)

import Data.Array as A
import Data.Either (Either(..))
import Data.Foreign (F, Foreign, ForeignError(..), toForeign, readString, readInt, readArray)
import Data.Foreign.Index ((!))
import Data.Foreign.Keys (keys)
import Data.List.NonEmpty (singleton)
import Data.Options (Options(..))
import Data.StrMap as StrMap
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

import Quill.API.Embed (Embed(..))
import Quill.API.Formats (Formats)

type Ops = Array Delta

readOps :: Foreign -> F Ops
readOps = (_ ! "ops") >=> readArray >=> traverse readDelta

opsToForeign :: Ops -> Foreign
opsToForeign ops = toForeign $
    StrMap.singleton "ops" (toForeign $ map deltaToForeign ops)

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

deltaToForeign :: Delta -> Foreign
deltaToForeign delta = toForeign $
    case delta of
        Insert (Right str) (Options fmts) ->
            StrMap.fromFoldable
                [ Tuple "insert" (toForeign str)
                , Tuple "attributes"
                    (toForeign $ StrMap.fromFoldable fmts)
                ]
        Insert (Left embed) (Options fmts) ->
            let embedObject (Image url) = StrMap.singleton "image" url
                embedObject (Video url) = StrMap.singleton "video" url
            in  StrMap.fromFoldable
                    [ Tuple "insert" (toForeign $ embedObject embed)
                    , Tuple "attributes"
                        (toForeign $ StrMap.fromFoldable fmts)
                    ]
        Delete n ->
            StrMap.singleton "delete" $ toForeign n

        Retain n (Options fmts) ->
            StrMap.fromFoldable $
                [ Tuple "retain" (toForeign n)
                , Tuple "attributes"
                    (toForeign $ StrMap.fromFoldable fmts)
                ]
