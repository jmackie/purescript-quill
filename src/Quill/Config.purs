module Quill.Config
    ( Config(..)
    , DebugLevel(..)
    , Theme(..)
    ) where

data Config = Config
    { debug :: DebugLevel
    , theme :: Theme
    }

data DebugLevel
    = DebugError  -- 'error'
    | DebugWarn   -- 'warn'
    | DebugLog    -- 'log'
    | DebugInfo   -- 'info'
    | DebugOff    -- false

data Theme
    = BubbleTheme  -- 'bubble' (builtin)
    | SnowTheme    -- 'snow'   (builtin)
    | CustomTheme String


