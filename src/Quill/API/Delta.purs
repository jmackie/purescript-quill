module Quill.API.Delta
    ( Ops(Ops)
    , Delta(Insert, Delete, Retain)
    )
where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(Left, Right))
import Data.List.NonEmpty as NonEmptyList
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Options (Options(Options))
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
import Foreign (F, Foreign, ForeignError(ForeignError))
import Foreign (readArray, readInt, readString) as Foreign
import Foreign.Class (class Encode, class Decode, encode, decode) as Foreign
import Foreign.Index (readProp) as Foreign
import Foreign.Keys (keys) as Foreign
import Foreign.Object as Object
import Quill.API.Embed (Embed(Image, Video))
import Quill.API.Formats (Formats)


-- | https://quilljs.com/docs/delta/
newtype Ops = Ops (Array Delta)

derive instance newtypeOps :: Newtype Ops _
instance foreignDecodeOps :: Foreign.Decode Ops where decode = decodeOps
instance foreignEncodeOps :: Foreign.Encode Ops where encode = encodeOps


decodeOps :: Foreign -> F Ops
decodeOps =
    Foreign.readProp "ops" >=>
    Foreign.readArray >=>
    traverse Foreign.decode >>>
    map wrap


encodeOps :: Ops -> Foreign
encodeOps =
    unwrap >>>
    map Foreign.encode >>>
    Object.singleton "ops" >>>
    Foreign.encode


-- | https://quilljs.com/docs/delta/
data Delta
    = Insert (Either Embed String) (Options Formats)
    | Delete Int
    | Retain Int (Options Formats)

instance foreignDecodeDelta :: Foreign.Decode Delta where decode = decodeDelta
instance foreignEncodeDelta :: Foreign.Encode Delta where encode = encodeDelta


decodeDelta :: Foreign -> F Delta
decodeDelta value = do
    keys <- Set.fromFoldable <$> Foreign.keys value
    decodeDelta' value keys


decodeDelta' :: Foreign -> Set String -> F Delta
decodeDelta' value keys
    | Set.member "insert" keys && Set.member "attributes" keys = do
        insert     <- Foreign.readProp "insert"     value >>= decodeDeltaInsert
        attributes <- Foreign.readProp "attributes" value >>= decodeOptions
        pure (Insert insert attributes)

    | Set.member "insert" keys = do
        insert <- Foreign.readProp "insert" value >>= decodeDeltaInsert
        pure (Insert insert mempty)

    | Set.member "delete" keys = do
        delete <- Foreign.readProp "delete" value >>= Foreign.readInt
        pure (Delete delete)

    | Set.member "retain" keys && Set.member "attributes" keys = do
        retain     <- Foreign.readProp "retain"     value >>= Foreign.readInt
        attributes <- Foreign.readProp "attributes" value >>= decodeOptions
        pure (Retain retain attributes)

    | Set.member "retain" keys = do
        retain <- Foreign.readProp "retain" value >>= Foreign.readInt
        pure (Retain retain mempty)

    | otherwise = do
        throwError <<< NonEmptyList.singleton <<< ForeignError $
        "unrecognised Delta properties: " <> show keys


decodeDeltaInsert :: Foreign -> F (Either Embed String)
decodeDeltaInsert value = string <|> image
  where
    string = Foreign.readString value <#> Right
    image  = Foreign.readProp "image" value >>= Foreign.readString >>> map (Image >>> Left)
    video  = Foreign.readProp "video" value >>= Foreign.readString >>> map (Video >>> Left)


encodeDelta :: Delta -> Foreign
encodeDelta delta = Foreign.encode $
    case delta of
         Insert (Right str) (Options fmts) ->
            Object.fromFoldable
                [ Tuple "insert"     (Foreign.encode str)
                , Tuple "attributes" (Foreign.encode $ Object.fromFoldable fmts)
                ]

         Insert (Left embed) (Options fmts) ->
            let embedObject (Image url) = Object.singleton "image" url
                embedObject (Video url) = Object.singleton "video" url
            in
            Object.fromFoldable
                [ Tuple "insert"     (Foreign.encode $ embedObject embed)
                , Tuple "attributes" (Foreign.encode $ Object.fromFoldable fmts)
                ]

         Delete n ->
            Object.singleton "delete" $ Foreign.encode n

         Retain n (Options fmts) ->
            Object.fromFoldable
                [ Tuple "retain"     (Foreign.encode n)
                , Tuple "attributes" (Foreign.encode $ Object.fromFoldable fmts)
                ]


decodeOptions :: forall opt. Foreign -> F (Options opt)
decodeOptions = Foreign.decode >>> map (Object.toUnfoldable >>> Options)
