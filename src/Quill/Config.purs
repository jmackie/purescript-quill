module Quill.Config
    ( Config(..)
    , configToForeign
    , DebugLevel(..)
    , Theme(..)
    ) where

import Prelude

import Data.Foreign (Foreign, toForeign)

data Config = Config
    { debug :: DebugLevel
    , theme :: Theme
    }

configToForeign :: Config -> Foreign
configToForeign (Config cfg) = toForeign $ cfg
    { debug = debugLevelToForeign cfg.debug
    , theme = themeToForeign cfg.theme
    }

data DebugLevel
    = DebugError  -- 'error'
    | DebugWarn   -- 'warn'
    | DebugLog    -- 'log'
    | DebugInfo   -- 'info'
    | DebugOff    -- false

debugLevelToForeign :: DebugLevel -> Foreign
debugLevelToForeign = case _ of
    DebugError -> toForeign "error"
    DebugWarn  -> toForeign "warn"
    DebugLog   -> toForeign "log"
    DebugInfo  -> toForeign "info"
    DebugOff   -> toForeign false

data Theme
    = BubbleTheme  -- 'bubble' (builtin)
    | SnowTheme    -- 'snow'   (builtin)
    | CustomTheme String

themeToForeign :: Theme -> Foreign
themeToForeign = toForeign <<< case _ of
    BubbleTheme      -> "bubble"
    SnowTheme        -> "snow"
    CustomTheme name -> name

