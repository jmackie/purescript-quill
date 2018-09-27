module Main where

import Prelude

import Color as Color
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Except.Trans (runExceptT)
import Data.Either (Either(Left, Right))
import Data.Foldable (fold, intercalate)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (wrap)
import Data.Options (Options, (:=))
import Data.String as S
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Exception as Exception
import Foreign as Foreign
import Quill.API.Content as QContent
import Quill.API.Delta as QDelta
import Quill.API.Events as QEvents
import Quill.API.Formats as QFormats
import Quill.API.Selection as QSelection
import Quill.API.Source as QSource
import Quill.Config as QConfig
import Quill.Editor as QEditor
import Web.DOM.NonElementParentNode as NonElementParentNode
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement (HTMLElement)
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window


main :: Effect Unit
main = do
    target <- getHTMLElementById "editor" >>= maybe (Exception.throw "no #editor element") pure
    result <- runExceptT (run target)
    case result of
         Right _ -> pure unit
         Left errors -> Exception.throw (renderMultipleErrors errors)


run :: forall m. MonadEffect m => MonadError Foreign.MultipleErrors m => HTMLElement -> m Unit
run element = do
    -- Initialise the editor
    editor <- QEditor.new editorConfig element

    -- Set some initial content
    let initialContent :: QDelta.Ops
        initialContent = wrap
            [ QDelta.Insert (Right "purescript-quill example\n") $
                QFormats.header := 1 <>
                QFormats.align  := QFormats.Center <>
                QFormats.color  := Color.fromInt 0xff0000
            , QDelta.Insert (Right "Hello World!") $
                QFormats.italic := true
            , QDelta.Insert (Right "\n")
                mempty
            ]

    _ <- QContent.setContents initialContent QSource.API editor

    -- Update initial content
    let updatedContent :: QDelta.Ops
        updatedContent = wrap
            [ QDelta.Retain (S.length "purescript-quill example\n") $
                mempty
            , QDelta.Retain (S.length "Hello World") $
                QFormats.italic := true
            , QDelta.Insert (Right " ")
                mempty
            , QDelta.Delete 1
            , QDelta.Insert (Right "?!")
                mempty
            , QDelta.Insert (Right "\n")
                mempty
            ]

    _ <- QContent.updateContents updatedContent QSource.API editor

    -- Insert a raw string at the end of the editor
    len <- QContent.getLength editor
    _   <- QContent.insertText len "I was inserted" mempty QSource.API editor

    -- Get the editors current text
    text <- QContent.getText { index: 0, length: Nothing } editor
    liftEffect (Console.log text)

    -- Select everything
    len' <- QContent.getLength editor
    QSelection.setSelection { index: 0, length: wrap len' } QSource.API editor

    -- Register callbacks
    QEvents.onTextChange
        (\_ _ _ -> Console.log   "callback: text-change")
        (\errs  -> Console.log $ "fallback: errors: " <> renderMultipleErrors errs)
        editor

    QEvents.onSelectionChange
        (\_ _ _ -> Console.log "callback: selection-change")
        QEvents.fallbackIgnore
        editor


editorConfig :: Options QConfig.Config
editorConfig = fold
    [ QConfig.debug       := QConfig.DebugWarn
    , QConfig.theme       := QConfig.SnowTheme
    , QConfig.placeholder := "Write here!"
    , QConfig.formats     :=
        [ QConfig.allow QFormats.bold
        , QConfig.allow QFormats.italic
        , QConfig.allow QFormats.underline
        , QConfig.allow QFormats.header
        , QConfig.allow QFormats.align
        , QConfig.allow QFormats.color
        ]
    ]


getHTMLElementById :: String -> Effect (Maybe HTMLElement)
getHTMLElementById id =
    HTML.window >>=
    Window.document >>=
    HTMLDocument.toNonElementParentNode >>>
    NonElementParentNode.getElementById id >>>
    map (_ >>= HTMLElement.fromElement)


renderMultipleErrors :: Foreign.MultipleErrors -> String
renderMultipleErrors = intercalate ", " <<< map Foreign.renderForeignError
