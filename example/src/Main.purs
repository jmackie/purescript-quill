module Main where

import Prelude

import Color as Color

import Control.Monad.Eff (Eff)
import Control.Monad.Except (runExcept)

import Data.Array as A
import Data.Either (Either(..), either)
import Data.Foreign (MultipleErrors, toForeign, renderForeignError)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Options ((:=))
import Data.String as S

import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (document)
import DOM.Node.ParentNode (QuerySelector(..), querySelector)
import DOM.Node.Types (Element)
import DOM.HTML.Types (HTMLElement, htmlDocumentToParentNode, readHTMLElement)

import Partial (crashWith)
import Partial.Unsafe (unsafePartial)

import Quill as Q
import Quill.API as QAPI
import Quill.API (runAPI)
import Quill.API.Delta (Delta(..))
import Quill.API.Formats as QFmt
import Quill.Config as QCfg
import Quill.Types (QUILL)

main :: forall e. Eff (dom :: DOM, quill :: QUILL | e) Unit
main = do
    target <- window
            >>= document
            >>= (htmlDocumentToParentNode
                >>> querySelector (QuerySelector "#editor"))
            <#> (_ >>= elementToHTMLElement)

    void $ case target of
        Just el -> runAPI do
            let cfg = QCfg.debug       := QCfg.DebugWarn
                   <> QCfg.theme       := QCfg.SnowTheme
                   <> QCfg.placeholder := "Write here!"
                   <> QCfg.formats     := [ QCfg.allow QFmt.bold
                                          , QCfg.allow QFmt.italic
                                          , QCfg.allow QFmt.underline
                                          , QCfg.allow QFmt.header
                                          , QCfg.allow QFmt.align
                                          , QCfg.allow QFmt.color
                                          ]
            editor <- Q.editor cfg el

            _ <- QAPI.setContents
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

            _ <- QAPI.updateContents
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

            pure unit

        Nothing -> unsafePartial $ crashWith "editor element not found!"

elementToHTMLElement :: Element -> Maybe HTMLElement
elementToHTMLElement =
        toForeign
    >>> readHTMLElement
    >>> runExcept
    >>> either (const Nothing) Just

renderMultipleErrors :: MultipleErrors -> String
renderMultipleErrors = S.joinWith ", " <<< A.fromFoldable <<< map renderForeignError

