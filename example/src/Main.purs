module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except (runExcept)

import Data.Array as A
import Data.Either (Either(..), either)
import Data.Foreign (MultipleErrors, toForeign, renderForeignError)
import Data.Maybe (Maybe(..))
import Data.Options ((:=))
import Data.String as S

import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (document)
import DOM.Node.ParentNode (querySelector, QuerySelector(..))
import DOM.Node.Types (Element)
import DOM.HTML.Types (HTMLElement, htmlDocumentToParentNode, readHTMLElement)

import Quill as Q
import Quill.API as API
import Quill.API.Formats as Formats
import Quill.API.Source as Source
import Quill.Config as QC
import Quill.Types (QUILL)

main :: forall e. Eff (console :: CONSOLE, dom :: DOM, quill :: QUILL | e) Unit
main = do
    target <- window
            >>= document
            >>= (htmlDocumentToParentNode
                >>> querySelector (QuerySelector "#editor"))
            <#> (_ >>= elementToHTMLElement)

    case target of
        Just el -> do
            let cfg = QC.debug       := QC.DebugWarn
                   <> QC.theme       := QC.SnowTheme
                   <> QC.placeholder := "Write here!"
                   <> QC.formats := [ QC.allow Formats.bold
                                    , QC.allow Formats.italic
                                    , QC.allow Formats.underline
                                    , QC.allow Formats.header
                                    ]
            editor <- Q.editor cfg el

            API.getLength editor
                >>= \end -> API.insertText
                    (either (const 0) id end)
                    "I'm bold"
                    (Formats.bold := true)
                    Source.API
                    editor
                >>= (case _ of
                    Left why  -> log $ "insertText failed: " <> renderMultipleErrors why
                    Right ops -> log $ "insertText deltas: " <> (show $ A.length ops))

            pure unit
        Nothing -> do
            log "editor not found!"
            pure unit

elementToHTMLElement :: Element -> Maybe HTMLElement
elementToHTMLElement =
        toForeign
    >>> readHTMLElement
    >>> runExcept
    >>> either (const Nothing) Just

renderMultipleErrors :: MultipleErrors -> String
renderMultipleErrors = S.joinWith ", " <<< A.fromFoldable <<< map renderForeignError
