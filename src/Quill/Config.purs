module Quill.Config
    ( Config(..)
    , defaultConfig
    , configToForeign
    , DebugLevel(..)
    , Theme(..)
    , Format(..)
    ) where

import Prelude

import Data.Foreign (Foreign, toForeign)
import Data.Maybe (Maybe(..), maybe)

import DOM.HTML.Types (HTMLElement)

-- | https://quilljs.com/docs/configuration/
type Config =
    { bounds      :: Maybe HTMLElement
    , debug       :: DebugLevel
    , formats     :: Array Format
    -- , modules :: not currently implemented
    , placeholder :: String
    , readonly    :: Boolean
    , strict      :: Boolean
    , theme       :: Theme
    }

-- | A `Config` instance with reasonable defaults
defaultConfig :: Config
defaultConfig =
    { bounds: Nothing
    , debug: DebugOff
    , formats: []
    , placeholder: ""
    , readonly: false
    , strict: true
    , theme: CustomTheme ""
    }

-- | Convert `Config` to it's javascript representation.
configToForeign :: Config -> Foreign
configToForeign cfg = toForeign $ cfg
    { bounds      = maybe null toForeign cfg.bounds -- TODO
    , debug       = debugLevelToForeign cfg.debug
    , formats     = toForeign $ formatToForeign <$> cfg.formats
    , placeholder = toForeign cfg.placeholder
    , readonly    = toForeign cfg.readonly
    , strict      = toForeign cfg.strict
    , theme       = themeToForeign cfg.theme
    }

foreign import null :: Foreign

-- | https://quilljs.com/docs/api/#debug
data DebugLevel
    = DebugError  -- 'error'
    | DebugWarn   -- 'warn'
    | DebugLog    -- 'log'
    | DebugInfo   -- 'info'
    | DebugOff    -- false

derive instance eqDebugLevel :: Eq DebugLevel

debugLevelToForeign :: DebugLevel -> Foreign
debugLevelToForeign = case _ of
    DebugError -> toForeign "error"
    DebugWarn  -> toForeign "warn"
    DebugLog   -> toForeign "log"
    DebugInfo  -> toForeign "info"
    DebugOff   -> toForeign false   -- because js

-- | https://quilljs.com/docs/themes/
data Theme
    = BubbleTheme  -- 'bubble' (builtin)
    | SnowTheme    -- 'snow'   (builtin)
    | CustomTheme String

derive instance eqTheme :: Eq Theme

themeToForeign :: Theme -> Foreign
themeToForeign = toForeign <<< case _ of
    BubbleTheme      -> "bubble"
    SnowTheme        -> "snow"
    CustomTheme name -> name

-- | https://quilljs.com/docs/formats/
data Format
    = BackgroundColor
    | Bold
    | Color
    | Font
    | InlineCode
    | Italic
    | Link
    | Size
    | Strikethrough
    | SuperscriptSubscript
    | Underline
    | Blockquote
    | Header
    | Indent
    | List
    | TextAlignment
    | TextDirection
    | CodeBlock
    | Formula
    | Image
    | Video

derive instance eqFormat :: Eq Format

formatToForeign :: Format -> Foreign
formatToForeign = toForeign <<< case _ of
    BackgroundColor      -> "background"
    Bold                 -> "bold"
    Color                -> "color"
    Font                 -> "font"
    InlineCode           -> "code"
    Italic               -> "italic"
    Link                 -> "link"
    Size                 -> "size"
    Strikethrough        -> "strike"
    SuperscriptSubscript -> "script"
    Underline            -> "underline"
    Blockquote           -> "blockquote"
    Header               -> "header"
    Indent               -> "indent"
    List                 -> "list"
    TextAlignment        -> "align"
    TextDirection        -> "direction"
    CodeBlock            -> "code-block"
    Formula              -> "formula"
    Image                -> "image"
    Video                -> "video"

