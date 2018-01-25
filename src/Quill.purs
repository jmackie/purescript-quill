module Quill
    ( editor
    ) where

import Control.Monad.Eff (Eff)

import Data.Function.Uncurried (Fn3, runFn3)
import Data.Foreign (Foreign)

import DOM.HTML.Types (HTMLElement)

import Quill.Config (Config, configToForeign)
import Quill.Types (QUILL, Quill, Editor)

editor :: forall eff. Config -> HTMLElement -> Eff (quill :: QUILL | eff) Editor
editor cfg el = runFn3 editorImpl quill el (configToForeign cfg)

foreign import quill :: Quill

foreign import editorImpl
    :: forall eff
     . Fn3 Quill HTMLElement Foreign (Eff (quill :: QUILL | eff) Editor)


