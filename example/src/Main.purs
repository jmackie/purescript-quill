module Main where

import Prelude

import Color as Color

import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except (runExcept)

import Data.Array as A
import Data.Either (Either(..), either)
import Data.Foreign (MultipleErrors, toForeign, renderForeignError)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Options ((:=))
import Data.String as S
import Data.Tuple (Tuple(..))

import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (document)
import DOM.Node.ParentNode (QuerySelector(..), querySelector)
import DOM.Node.Types (Element)
import DOM.HTML.Types (HTMLElement, htmlDocumentToParentNode, readHTMLElement)

import Partial (crashWith)
import Partial.Unsafe (unsafePartial)

import Quill as Q
import Quill.API (runAPI)
import Quill.API as QAPI
import Quill.API.Delta (Delta(..))
import Quill.API.Formats as QFmt
import Quill.API.Source as QSource
import Quill.Config as QCfg
import Quill.Types (QUILL)

main :: forall e. Eff (console :: CONSOLE, dom :: DOM, quill :: QUILL | e) Unit
main = do
    target <- window
            >>= document
            >>= (htmlDocumentToParentNode
                >>> querySelector (QuerySelector "#editor"))
            <#> (_ >>= elementToHTMLElement)
    result <- runAPI $ case target of
        Nothing ->
            unsafePartial $ crashWith "editor element not found!"
        Just el -> do
            -- Initialise the editor
            let cfg = QCfg.debug       := QCfg.DebugWarn
                   <> QCfg.theme       := QCfg.SnowTheme
                   <> QCfg.placeholder := "Write here!"
                   <> QCfg.formats     :=
                        [ QCfg.allow QFmt.bold
                        , QCfg.allow QFmt.italic
                        , QCfg.allow QFmt.underline
                        , QCfg.allow QFmt.header
                        , QCfg.allow QFmt.align
                        , QCfg.allow QFmt.color
                        ]
            editor <- Q.editor cfg el

            -- Set some initial content
            void $ QAPI.setContents
                        [ Insert (Right "purescript-quill example\n") $
                            QFmt.header := 1 <>
                            QFmt.align  := QFmt.Center <>
                            QFmt.color  := Color.fromInt 0xff0000
                        , Insert (Right "Hello World!") $
                            QFmt.italic := true
                        , Insert (Right "\n") mempty
                        ]
                        Nothing
                        editor

            -- Update initial content
            void $ QAPI.updateContents
                        [ Retain (S.length "purescript-quill example\n") $
                            mempty
                        , Retain (S.length "Hello World") $
                            QFmt.italic := true
                        , Insert (Right " ") mempty
                        , Delete 1
                        , Insert (Right "?!") mempty
                        , Insert (Right "\n") mempty
                        ]
                        Nothing
                        editor

            -- Insert a raw string at the end of the editor
            void $ QAPI.getLength editor >>= \len ->
                   QAPI.insertText
                        len
                        "I was inserted"
                        mempty
                        QSource.API
                        editor

            -- Get the editors current text
            text <- QAPI.getText
                        0
                        Nothing -- use default
                        editor
            liftEff $ log text

            -- Select everything
            void $ QAPI.getLength editor >>= \len ->
                   QAPI.setSelection
                        (Tuple 0 len)
                        QSource.API
                        editor

            pure unit

    case result of
        Left errs ->
            unsafePartial $ crashWith $ "API call failed: " <> renderMultipleErrors errs
        Right _ ->
            pure unit


    pure unit


elementToHTMLElement :: Element -> Maybe HTMLElement
elementToHTMLElement =
        toForeign
    >>> readHTMLElement
    >>> runExcept
    >>> either (const Nothing) Just

renderMultipleErrors :: MultipleErrors -> String
renderMultipleErrors = S.joinWith ", " <<< A.fromFoldable <<< map renderForeignError

