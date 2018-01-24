module Quill
    ( Editor
    , editor
    ) where

import Prelude

import Control.Monad.Eff (Eff)

import Data.Foreign (Foreign, toForeign)
import Data.Function.Uncurried (Fn3, runFn3)

import DOM.HTML.Types (HTMLElement)

import Quill.Config (Config(..))
import Quill.Config as Config
import Quill.Types (QUILL, Quill)

data Editor = Editor

foreign import quill :: Quill

editor :: forall eff. Config -> HTMLElement -> Eff (quill :: QUILL | eff) Editor
editor cfg el = runFn3 editorImpl quill el (configToForeign cfg)

foreign import editorImpl
    :: forall eff
     . Fn3 Quill HTMLElement Foreign (Eff (quill :: QUILL | eff) Editor)


configToForeign :: Config -> Foreign
configToForeign (Config cfg) = toForeign $ cfg
    { debug = debugLevelToForeign cfg.debug
    , theme = themeToForeign cfg.theme
    }

debugLevelToForeign :: Config.DebugLevel -> Foreign
debugLevelToForeign = case _ of
    Config.DebugError -> toForeign "error"
    Config.DebugWarn  -> toForeign "warn"
    Config.DebugLog   -> toForeign "log"
    Config.DebugInfo  -> toForeign "info"
    Config.DebugOff   -> toForeign false

themeToForeign :: Config.Theme -> Foreign
themeToForeign = toForeign <<< case _ of
    Config.BubbleTheme      -> "bubble"
    Config.SnowTheme        -> "snow"
    Config.CustomTheme name -> name
