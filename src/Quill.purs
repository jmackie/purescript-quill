module Quill
    ( Editor
    , editor
    ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)

import Data.Foreign (Foreign)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Options (Options, options)

import DOM.HTML.Types (HTMLElement)

import Quill.API.API (API)
import Quill.Config (Config)
import Quill.Types (QUILL, Quill)

--------------------------------------------------------------------------------
-- | Initialise a Quill `Editor` on the given element.
editor
    :: forall eff
     . Options Config
    -> HTMLElement
    -> API (quill :: QUILL | eff) Editor
editor cfg el =
    liftEff $ runFn3 editorImpl
        quill
        el
        (options cfg)

foreign import editorImpl
    :: forall eff
     . Fn3
        Quill        -- quill
        HTMLElement  -- el
        Foreign      -- cfg
        (Eff (quill :: QUILL | eff) Editor)

--------------------------------------------------------------------------------
-- | An instance of the `Quill` javascript object.
data Editor = Editor

--------------------------------------------------------------------------------

foreign import quill :: Quill  -- from quilljs

