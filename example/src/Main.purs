module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except (runExcept)

import Data.Either (Either(..), either)
import Data.Foreign (toForeign)
import Data.Maybe (Maybe(..))

import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (document)
import DOM.Node.ParentNode (querySelector, QuerySelector(..))
import DOM.Node.Types (Element)
import DOM.HTML.Types (HTMLElement, htmlDocumentToParentNode, readHTMLElement)

import Quill as Q
import Quill.Config as QC
import Quill.Types (QUILL)
import Quill.API.Content as API

main :: forall e. Eff (console :: CONSOLE, dom :: DOM, quill :: QUILL | e) Unit
main = do
    target <- window
            >>= document
            >>= (htmlDocumentToParentNode
                >>> querySelector (QuerySelector "#editor"))
            <#> (_ >>= elementToHTMLElement)

    case target of
        Just el -> do
            let cfg = QC.defaultConfig
                        { debug = QC.DebugWarn
                        , theme = QC.SnowTheme
                        , formats =
                             [ QC.Header
                             , QC.Bold
                             , QC.Italic
                             , QC.Underline
                             , QC.Link
                             , QC.List
                             ]
                        , placeholder = "Write here!"
                        }
            editor <- Q.editor cfg el

            API.getLength editor >>= (case _ of
                 Left _    -> log   "getLength failed!"
                 Right len -> log $ "editor length is: " <> show len)

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

