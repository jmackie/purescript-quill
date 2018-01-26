module Quill.API.FormatSpec
    ( FormatSpec(..)
    , readFormatSpec
    , Alignment(..)
    , FontName
    ) where

import Prelude

import Color (Color, fromHexString)

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (except)

import Data.Either (note)
import Data.Foreign (F, Foreign, ForeignError(..),
                     readString, readBoolean, readNumber, readInt)
import Data.List.NonEmpty (singleton)
import Data.Tuple (Tuple(..))

-- | https://quilljs.com/docs/formats/
data FormatSpec
    = BackgroundColor Color
    | TextColor Color
    | Bold Boolean
    | Font FontName
    | InlineCode Boolean
    | Italic Boolean
    | Link String
    | Size Number
    | Strikethrough Boolean
    -- TODO: | Superscript
    -- TODO: | Subscript
    | Underline Boolean
    | Blockquote Boolean
    | Header Int
    | Indent Int
    -- TODO: | List
    | TextAlignment Alignment
    -- TODO: | TextDirection
    | CodeBlock Boolean

-- | Options for text alignment.
data Alignment
    = Left
    | Center
    | Right
    | Justify

-- | E.g. "sans-serif"
type FontName = String

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

        "color" -> do
            str   <- readString value
            color <- except $ fromHexString str # note (singleton
                        (ErrorAtProperty key $
                         ForeignError $ "invalid hex color string: " <> str))
            pure $ TextColor color

        "bold"       -> readBoolean value   <#> Bold
        "font"       -> readString value    <#> Font
        "code"       -> readBoolean value   <#> InlineCode
        "italic"     -> readBoolean value   <#> Italic
        "link"       -> readString value    <#> Link
        "size"       -> readNumber value    <#> Size
        "strike"     -> readBoolean value   <#> Strikethrough
        "underline"  -> readBoolean value   <#> Underline
        "blockquote" -> readBoolean value   <#> Blockquote
        "header"     -> readInt value       <#> Header
        "indent"     -> readInt value       <#> Indent
        "align"      -> readAlignment value <#> TextAlignment
        "code-block" -> readBoolean value   <#> CodeBlock

        -- Failure case
        prop -> do
            throwError $ (singleton
                (ErrorAtProperty key $
                 ForeignError $ "unknown property"))

readAlignment :: Foreign -> F Alignment
readAlignment value = do
    str <- readString value
    case str of
         "left"    -> pure Left
         "center"  -> pure Center
         "right"   -> pure Right
         "justify" -> pure Justify
         _ ->
            throwError $ (singleton
                (ForeignError $ "unknown alignment: " <> str))

