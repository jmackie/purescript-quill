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

-- | Initial Quill editor configuration options.
-- |
-- | ### `bounds`
-- |
-- | DOM Element or a CSS selector for a DOM Element, within which the editor’s
-- | ui elements (i.e. tooltips, etc.) should be confined. Currently,
-- | it only considers left and right boundaries.
-- |
-- | ### `debug`
-- |
-- | Level for logging messages.
-- |
-- | ### `formats`
-- |
-- | Whitelist of formats to allow in the editor.
-- |
-- | ### `placeholder`
-- |
-- | Placeholder text to show when editor is empty.
-- |
-- | ### `readonly`
-- |
-- | Whether to instantiate the editor to read-only mode.
-- |
-- | ### `strict`
-- |
-- | Some improvements and modifications, under a strict interpretation of
-- | semver, would warrant a major version bump. To prevent small changes
-- | from inflating Quill’s version number, they are disabled by this
-- | strict flag. Specific changes can be found in the Changelog and
-- | searching for “strict”. Setting this to false opts into potential
-- | future improvements.
-- |
-- | ### `theme`
-- |
-- | Name of theme to use. The builtin options are "bubble" or "snow". An
-- | invalid or falsy value will load a default minimal theme. Note the
-- | theme’s specific stylesheet still needs to be included manually. See
-- | Themes for more information.
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

