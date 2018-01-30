module Quill.API.Content
    ( deleteText
    , getContents
    , getLength
    , getText
    , insertEmbed
    , insertText
    ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Except (runExcept)

import Data.Function.Uncurried (Fn1, runFn1, Fn3, runFn3,
                                Fn4, runFn4, Fn5, runFn5)

import Data.Foreign (Foreign, toForeign, readString, readInt)
import Data.Maybe (Maybe, maybe)
import Data.Options (Options, options)

import Quill (Editor)
import Quill.API.Delta (Ops, readOps)
import Quill.API.Embed (Embed(..))
import Quill.API.Formats (Formats)
import Quill.API.Return (Return)
import Quill.API.Source (Source)
import Quill.Types (QUILL)

-- | https://quilljs.com/docs/api/#deletetext
-- | NOTE: I think this might actually return an array of Deltass...
deleteText
    :: forall eff
     . Int -> Int -> Source -> Editor -> Eff (quill :: QUILL | eff) (Return Ops)
deleteText index length source editor = runExcept <<< readOps <$> runFn4 deleteTextImpl
    editor index length (show source)

foreign import deleteTextImpl
    :: forall eff
     . Fn4 Editor Int Int String (Eff (quill :: QUILL | eff) Foreign)

-- | https://quilljs.com/docs/api/#getcontents
getContents
    :: forall eff
     . Int -> (Maybe Int) -> Editor -> Eff (quill :: QUILL | eff) (Return Ops)
getContents index length editor = runExcept <<< readOps <$> runFn3 getContentsImpl
    editor index (maybe null toForeign length)

foreign import getContentsImpl
    :: forall eff
     . Fn3 Editor Int Foreign (Eff (quill :: QUILL | eff) Foreign)

-- | https://quilljs.com/docs/api/#getlength
getLength
    :: forall eff
     . Editor -> Eff (quill :: QUILL | eff) (Return Int)
getLength editor = runExcept <<< readInt <$> runFn1 getLengthImpl
    editor

foreign import getLengthImpl
    :: forall eff
     . Fn1 Editor (Eff (quill :: QUILL | eff) Foreign)

-- | https://quilljs.com/docs/api/#gettext
getText
    :: forall eff
     . Int -> (Maybe Int) -> Editor -> Eff (quill :: QUILL | eff) (Return String)
getText index length editor = runExcept <<< readString <$> runFn3 getTextImpl
    editor index (maybe null toForeign length)

foreign import getTextImpl
    :: forall eff
     . Fn3 Editor Int Foreign (Eff (quill :: QUILL | eff) Foreign)

-- | https://quilljs.com/docs/api/#insertembed
insertEmbed
    :: forall eff
     . Int -> Embed -> Source -> Editor -> Eff (quill :: QUILL | eff) (Return Ops)
insertEmbed index (Image url) source editor = runExcept <<< readOps <$> runFn5 insertEmbedImpl
    editor index "image" url (show source)
insertEmbed index (Video url) source editor = runExcept <<< readOps <$> runFn5 insertEmbedImpl
    editor index "video" url (show source)

foreign import insertEmbedImpl
    :: forall eff
     . Fn5 Editor Int String String String (Eff (quill :: QUILL | eff) Foreign)

-- | https://quilljs.com/docs/api/#inserttext
insertText
    :: forall eff
     . Int -> String -> Options Formats -> Source -> Editor -> Eff (quill :: QUILL | eff) (Return Ops)
insertText index text formats source editor = runExcept <<< readOps <$> runFn5 insertTextImpl
    editor index text (options formats) (show source)

foreign import insertTextImpl
    :: forall eff
     . Fn5 Editor Int String Foreign String (Eff (quill :: QUILL | eff) Foreign)

foreign import null :: Foreign

