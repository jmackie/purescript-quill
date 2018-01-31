module Quill.API.Events where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)

import Data.Function.Uncurried (Fn2, runFn2, Fn3, mkFn3)

import Quill (Editor)
import Quill.API.API (API)
import Quill.API.Delta (Ops, readOps)
import Quill.API.Range (Range, readRange)
import Quill.API.Source (Source, readSource)
import Quill.Types (QUILL)

type Callback = Ops -> Ops -> Source

--------------------------------------------------------------------------------
-- | https://quilljs.com/docs/api/#text-change
onTextChange
    :: forall a eff
     . (Ops -> Ops -> Source -> a)
    -> Editor
    -> API (quill :: QUILL | eff) Unit
onTextChange callback editor = do
    let callback' a b c = do
            delta <- readOps a
            oldContents <- readOps b
            source <- readSource c
            pure $ callback delta oldContents source
    void $ liftEff $ runFn2 onTextChangeImpl editor (mkFn3 callback')

foreign import onTextChangeImpl
    :: forall a b c d eff
     . Fn2
        Editor        -- self
        (Fn3 a b c d) -- callback
        (Eff (quill :: QUILL | eff) Unit)

--------------------------------------------------------------------------------
-- | https://quilljs.com/docs/api/#selection-change
onSelectionChange
    :: forall a eff
     . (Range -> Range -> Source -> a)
    -> Editor
    -> API (quill :: QUILL | eff) Unit
onSelectionChange callback editor = do
    let callback' a b c = do
            range <- readRange a
            oldRange <- readRange b
            source <- readSource c
            pure $ callback range oldRange source
    void $ liftEff $ runFn2 onSelectionChangeImpl editor (mkFn3 callback')

foreign import onSelectionChangeImpl
    :: forall a b c d eff
     . Fn2
        Editor        -- self
        (Fn3 a b c d) -- callback
        (Eff (quill :: QUILL | eff) Unit)

