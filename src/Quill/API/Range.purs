module Quill.API.Range
    ( Range
    , decodeOpenRange
    , decodeClosedRange
    )
where

import Prelude

import Control.Monad.Error.Class (try)
import Data.Either (Either(Left, Right))
import Data.Identity (Identity)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (wrap)
import Foreign (Foreign, F)
import Foreign (readInt) as Foreign
import Foreign.Index (readProp) as Foreign


type Range f = { index :: Int, length :: f Int }


decodeOpenRange :: Foreign -> F (Range Maybe)
decodeOpenRange value = do
    index  <- Foreign.readProp "index"  value >>= Foreign.readInt
    try (Foreign.readProp "length" value) >>= case _ of
        Left _       -> pure { index, length: Nothing }
        Right length -> { index, length:_ } <<< Just <$> Foreign.readInt length


decodeClosedRange :: Foreign -> F (Range Identity)
decodeClosedRange value = do
    index  <- Foreign.readProp "index"  value >>= Foreign.readInt
    length <- Foreign.readProp "length" value >>= Foreign.readInt
    pure { index, length: wrap length }
