module Quill.API.Range
    ( Range(..)
    , readRange
    , Index
    , index
    , Length
    , length
    ) where

import Prelude

import Data.Foreign (Foreign, F, readInt)
import Data.Foreign.Index ((!))
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..), fst, snd)

newtype Range = Range (Tuple Index Length)

derive instance newtypeRange :: Newtype Range _

readRange :: Foreign -> F Range
readRange value = do
    i   <- value ! "index"  >>= readInt
    len <- value ! "length" >>= readInt
    pure $ Range $ Tuple i len

type Index = Int

index :: Range -> Index
index = unwrap >>> fst

type Length = Int

length :: Range -> Length
length = unwrap >>> snd

