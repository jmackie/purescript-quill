module Quill.API.Range
    ( Range(..)
    , Index
    , Length
    , mkRange
    , readRange
    , index
    , length
    ) where

import Prelude

import Data.Foreign (Foreign, F, readInt)
import Data.Foreign.Index ((!))
import Data.Tuple (Tuple(..), fst, snd)

type Range = Tuple Index Length

mkRange :: Index -> Length -> Range
mkRange = Tuple

readRange :: Foreign -> F Range
readRange value = Tuple
    <$> (value ! "index"  >>= readInt)
    <*> (value ! "length" >>= readInt)

type Index = Int

index :: Range -> Index
index = fst

type Length = Int

length :: Range -> Length
length = snd

