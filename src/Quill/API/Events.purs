module Quill.API.Events
    ( onTextChange
    , onSelectionChange
    , fallbackIgnore
    ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except (runExcept)

import Data.Either (Either(..))
import Data.Foreign (Foreign, MultipleErrors)
import Data.Function.Uncurried (Fn2, runFn2)

import Quill (Editor)
import Quill.API.API (API)
import Quill.API.Delta (Ops, readOps)
import Quill.API.Range (Range, readRange)
import Quill.API.Source (Source, readSource)
import Quill.Types (QUILL)

--------------------------------------------------------------------------------
-- | https://quilljs.com/docs/api/#text-change
onTextChange
    :: forall eff
     . (Ops -> Ops -> Source -> Eff eff Unit)
    -> (MultipleErrors -> Eff eff Unit)
    -> Editor
    -> API (quill :: QUILL | eff) Unit
onTextChange callback fallback editor = do
    let callback' a b c = do
            let args = runExcept $ { delta:_, oldContents:_, source:_ }
                    <$> readOps a
                    <*> readOps b
                    <*> readSource c
            case args of
                Right { delta, oldContents, source } ->
                    callback delta oldContents source
                Left errs ->
                    fallback errs

    liftEff $ runFn2 onTextChangeImpl editor callback'

foreign import onTextChangeImpl
    :: forall eff
     . Fn2
        Editor
        (Foreign -> Foreign -> Foreign -> Eff eff Unit)
        (Eff (quill :: QUILL | eff) Unit)

--------------------------------------------------------------------------------
-- | https://quilljs.com/docs/api/#selection-change
onSelectionChange
    :: forall eff
     . (Range -> Range -> Source -> Eff eff Unit)
    -> (MultipleErrors -> Eff eff Unit)
    -> Editor
    -> API (quill :: QUILL | eff) Unit
onSelectionChange callback fallback editor = do
    let callback' a b c = do
            let args = runExcept $ { range:_, oldRange:_, source:_ }
                    <$> readRange a
                    <*> readRange b
                    <*> readSource c
            case args of
                Right { range, oldRange, source } ->
                    callback range oldRange source
                Left errs ->
                    fallback errs
    liftEff $ runFn2 onSelectionChangeImpl editor callback'

foreign import onSelectionChangeImpl
    :: forall eff
     . Fn2
        Editor
        (Foreign -> Foreign -> Foreign -> Eff eff Unit)
        (Eff (quill :: QUILL | eff) Unit)

--------------------------------------------------------------------------------

fallbackIgnore :: forall eff. MultipleErrors -> Eff eff Unit
fallbackIgnore = const (pure unit)

