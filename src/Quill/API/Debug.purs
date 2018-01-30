module Quill.API.Debug (Debug(..)) where

-- | https://quilljs.com/docs/api/#debug
data Debug
    = DebugError  -- 'error'
    | DebugWarn   -- 'warn'
    | DebugLog    -- 'log'
    | DebugInfo   -- 'info'
    | DebugOff    -- false
