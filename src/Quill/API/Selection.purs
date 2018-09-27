module Quill.API.Selection
    ( getBounds
    , Bounds
    , getSelection
    , setSelection
    )
where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Identity (Identity)
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (unwrap)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried as UncurriedEffect
import Foreign (Foreign, F)
import Foreign (MultipleErrors, readNumber) as Foreign
import Foreign.Index (readProp) as Foreign
import Quill.API.Range (Range, decodeClosedRange)
import Quill.API.Source (Source)
import Quill.Editor (Editor)


-- | https://quilljs.com/docs/api/#getbounds
getBounds
    :: forall m
     . MonadEffect m
    => MonadError Foreign.MultipleErrors m
    => Range Maybe
    -> Editor
    -> m Bounds
getBounds { index, length } editor =
    either throwError pure <<< runExcept <<< decodeBounds <=< liftEffect $
    UncurriedEffect.runEffectFn3 getBoundsImpl
        editor index (fromMaybe 0 length)


foreign import getBoundsImpl
    :: UncurriedEffect.EffectFn3
            Editor
            Int  -- index
            Int  -- length
            Foreign


-- | https://quilljs.com/docs/api/#getselection
getSelection
    :: forall m
     . MonadEffect m
    => MonadError Foreign.MultipleErrors m
    => Boolean
    -> Editor
    -> m (Range Identity)
getSelection focus editor =
    either throwError pure <<< runExcept <<< decodeClosedRange <=< liftEffect $
    UncurriedEffect.runEffectFn2 getSelectionImpl
        editor focus

foreign import getSelectionImpl
    :: UncurriedEffect.EffectFn2
            Editor
            Boolean -- focus
            Foreign


-- | https://quilljs.com/docs/api/#setselection
setSelection
    :: forall m
     . MonadEffect m
    => MonadError Foreign.MultipleErrors m
    => Range Identity
    -> Source
    -> Editor
    -> m Unit
setSelection { index, length } source editor =
    liftEffect $ UncurriedEffect.runEffectFn4 setSelectionImpl
        editor index (unwrap length) (show source)

foreign import setSelectionImpl
    :: UncurriedEffect.EffectFn4
            Editor
            Int    -- index
            Int    -- length
            String -- source
            Unit


type Bounds =
    { left   :: Number
    , top    :: Number
    , height :: Number
    , width  :: Number
    }


decodeBounds :: Foreign -> F Bounds
decodeBounds value =  do
    left   <- Foreign.readProp "left"   value >>= Foreign.readNumber
    top    <- Foreign.readProp "top"    value >>= Foreign.readNumber
    height <- Foreign.readProp "height" value >>= Foreign.readNumber
    width  <- Foreign.readProp "width"  value >>= Foreign.readNumber
    pure { left, top, height, width }
