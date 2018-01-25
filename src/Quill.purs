module Quill
    ( Editor
    , editor
    ) where

import Control.Monad.Eff (Eff)

import Data.Foreign (Foreign)
import Data.Function.Uncurried (Fn3, runFn3)

import DOM.HTML.Types (HTMLElement)

import Quill.Config (Config, configToForeign)
import Quill.Types (QUILL, Quill)

foreign import quill :: Quill  -- from quilljs

-- | Initialise a Quill `Editor` on the given element.
editor :: forall eff. Config -> HTMLElement -> Eff (quill :: QUILL | eff) Editor
editor cfg el = runFn3 editorImpl quill el (configToForeign cfg)

foreign import editorImpl
    :: forall eff
     . Fn3 Quill HTMLElement Foreign (Eff (quill :: QUILL | eff) Editor)

-- | An instance of the `Quill` javascript object.
data Editor = Editor


