module Quill.API.Formats
    ( Formats
    , SingleFormat
    , Alignment(..)
    , FontName
    , singleFormat
    , name
    , value
    , background
    , color
    , bold
    , font
    , code
    , italic
    , link
    , size
    , strike
    , underline
    , blockquote
    , header
    , indent
    , align
    , codeBlock
    ) where

import Prelude

import Color (Color, toHexString)

import Data.Array (head)
import Data.Foreign (Foreign, toForeign)
import Data.Maybe (fromJust)
import Data.Newtype (unwrap)
import Data.Op (Op(..))
import Data.Options (Options(..), Option, opt, assoc)
import Data.Tuple (Tuple(..))

import Partial.Unsafe (unsafePartial)

-- | https://quilljs.com/docs/formats/
data Formats

background :: Option Formats Color
background = optWith toHexString "background"

color :: Option Formats Color
color = optWith toHexString "color"

bold :: Option Formats Boolean
bold = opt "bold"

font :: Option Formats FontName
font = opt "font"

code :: Option Formats Boolean
code = opt "code"

italic :: Option Formats Boolean
italic = opt "italic"

link :: Option Formats Boolean
link = opt "link"

size :: Option Formats Number
size = opt "size"

strike :: Option Formats Boolean
strike = opt "strike"

underline :: Option Formats Boolean
underline = opt "underline"

blockquote :: Option Formats Boolean
blockquote = opt "blockquote"

header :: Option Formats Int
header = opt "header"

indent :: Option Formats Int
indent = opt "indent"

align :: Option Formats Alignment
align = optWith show "align"

codeBlock :: Option Formats Boolean
codeBlock = opt "code-block"

-- | Text alignment.
data Alignment
    = Left
    | Center
    | Right
    | Justify

instance showAlignment :: Show Alignment where
    show Left   = "left"
    show Center = "center"
    show Right  = "right"
    show _      = "justify" -- HACK: see `optWith` comment below

newtype SingleFormat = SingleFormat (Tuple String Foreign)

singleFormat :: forall a. Option Formats a -> a -> SingleFormat
singleFormat o v = SingleFormat $ unsafePartial $ fromJust $ head $ unwrap $ assoc o v

name :: SingleFormat -> String
name (SingleFormat (Tuple n _)) = n

value :: SingleFormat -> Foreign
value (SingleFormat (Tuple _ v)) = v

-- | E.g. "sans-serif"
type FontName = String

-- NOTE: uses of `optWith` here need to handle the case that the function
-- recieves an unsafely coerced unit value, as part of initial configuration.
-- For whatever reason, `Color.toHexString` seems robust to this.
optWith :: forall opt a b . (a -> b) -> String -> Option opt a
optWith f = Op <<< \k v -> Options [Tuple k (toForeign $ f v)]

