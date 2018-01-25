module Quill.API.FormatSpec
    ( FormatSpec(..)
    , readFormatSpec
    , Alignment(..)
    , FontName
    , URL
    ) where

import Prelude

import Color (Color, fromHexString)

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (except)

import Data.Either (note)
import Data.Foreign (F, Foreign, ForeignError(..), readString)
import Data.List.NonEmpty (singleton)
import Data.Tuple (Tuple(..))

-- | https://quilljs.com/docs/formats/
data FormatSpec
    = BackgroundColor Color
    | Bold Boolean
    | TextColor Color
    | Font FontName
    | InlineCode Boolean
    | Italic Boolean
    | Link String
    | Size Number
    | Strikethrough Boolean
    | Superscript Boolean
    | Subscript Boolean
    | Underline Boolean
    | Blockquote Boolean
    | Header Int
    | Indent Int
    -- TODO | List
    | TextAlignment Alignment
    | TextDirection Boolean
    | CodeBlock Boolean
    -- TODO | Formula
    | Image URL
    | Video URL

-- | Options for text alignment.
data Alignment
    = Left
    | Center
    | Right
    | Justify

-- | E.g. "sans-serif"
type FontName = String

-- | E.g. "https://pursuit.purescript.org/"
type URL = String

-- | Attempt to read in a `FormatSpec` from an object key and associated value.
readFormatSpec :: Tuple String Foreign -> F FormatSpec
readFormatSpec (Tuple key value) =
    case key of
        "background" -> do
            str   <- readString value
            color <- except $ fromHexString str # note (singleton
                        (ErrorAtProperty key $
                         ForeignError $ "invalid hex color string: " <> str))
            pure $ BackgroundColor color

        prop -> do
            throwError $ (singleton
                (ErrorAtProperty key $
                 ForeignError $ "unknown property"))

