module Quill.API.Events
    ( onTextChange
    , onSelectionChange
    , fallbackIgnore
    ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either(Left, Right))
import Data.Identity (Identity)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried as UncurriedEffect
import Foreign (Foreign)
import Foreign (MultipleErrors) as Foreign
import Foreign.Class (decode) as Foreign
import Quill.API.Delta (Ops)
import Quill.API.Range (Range, decodeClosedRange)
import Quill.API.Source (Source)
import Quill.Editor (Editor)


-- | https://quilljs.com/docs/api/#text-change
onTextChange
    :: forall m
     . MonadEffect m
    => (Ops -> Ops -> Source -> Effect Unit)
    -> (Foreign.MultipleErrors -> Effect Unit)
    -> Editor
    -> m Unit
onTextChange callback fallback editor =
    liftEffect (UncurriedEffect.runEffectFn2 onTextChangeImpl editor callback')
  where
    callback' :: Foreign -> Foreign -> Foreign -> Effect Unit
    callback' a b c =
        let args = runExcept do
                    delta       <- Foreign.decode a
                    oldContents <- Foreign.decode b
                    source      <- Foreign.decode c
                    pure { delta, oldContents, source }
        in
        case args of
             Right { delta, oldContents, source } ->
                callback delta oldContents source
             Left errs ->
                fallback errs

foreign import onTextChangeImpl
    :: UncurriedEffect.EffectFn2
            Editor
            (Foreign -> Foreign -> Foreign -> Effect Unit)
            Unit


-- | https://quilljs.com/docs/api/#selection-change
onSelectionChange
    :: forall m
     . MonadEffect m
    => (Range Identity -> Range Identity -> Source -> Effect Unit)
    -> (Foreign.MultipleErrors -> Effect Unit)
    -> Editor
    -> m Unit
onSelectionChange callback fallback editor = do
    liftEffect (UncurriedEffect.runEffectFn2 onSelectionChangeImpl editor callback')
  where
    callback' :: (Foreign -> Foreign -> Foreign -> Effect Unit)
    callback' a b c =
        let args = runExcept do
                    range    <- decodeClosedRange a
                    oldRange <- decodeClosedRange b
                    source   <- Foreign.decode c
                    pure { range, oldRange, source }
        in
        case args of
             Right { range, oldRange, source } ->
                callback range oldRange source
             Left errs ->
                fallback errs

foreign import onSelectionChangeImpl
    :: UncurriedEffect.EffectFn2
            Editor
            (Foreign -> Foreign -> Foreign -> Effect Unit)
            Unit


fallbackIgnore :: Foreign.MultipleErrors -> Effect Unit
fallbackIgnore = const (pure unit)
