module Quill.API.Content
    ( deleteText
    , getContents
    , getLength
    , getText
    , insertEmbed
    , insertText
    , setContents
    , setText
    , updateContents
    ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Except (runExcept)

import Data.Function.Uncurried (Fn1, runFn1, Fn3, runFn3,
                                Fn4, runFn4, Fn5, runFn5)

import Data.Foreign (Foreign, readString, readInt)
import Data.Maybe (Maybe, fromMaybe)
import Data.Options (Options, options)

import Quill (Editor)
import Quill.API.Delta (Ops, readOps, opsToForeign)
import Quill.API.Embed (Embed(..))
import Quill.API.Formats (Formats)
import Quill.API.Return (Return)
import Quill.API.Source (Source)
import Quill.API.Source as Source
import Quill.Types (QUILL)

import Unsafe.Coerce (unsafeCoerce)

type DefaultArg = Maybe

-- | https://quilljs.com/docs/api/#deletetext
-- | NOTE: I think this might actually return an array of Deltass...
deleteText
    :: forall eff
     . Int
    -> Int
    -> DefaultArg Source
    -> Editor
    -> Eff (quill :: QUILL | eff) (Return Ops)
deleteText index length source editor =
    runExcept <<< readOps <$> runFn4 deleteTextImpl
        editor
        index
        length
        (fromMaybe Source.API source # show)

foreign import deleteTextImpl
    :: forall eff
     . Fn4
        Editor -- self
        Int    -- index
        Int    -- length
        String -- source
        (Eff (quill :: QUILL | eff) Foreign)

-- | https://quilljs.com/docs/api/#getcontents
getContents
    :: forall eff
     . Int
    -> DefaultArg Int
    -> Editor
    -> Eff (quill :: QUILL | eff) (Return Ops)
getContents index length editor =
    runExcept <<< readOps <$> runFn3 getContentsImpl
        editor
        index
        (fromMaybe (unsafeCoerce undefined) length)

foreign import getContentsImpl
    :: forall eff
     . Fn3
        Editor -- self
        Int    -- index
        Int    -- length
        (Eff (quill :: QUILL | eff) Foreign)

-- | https://quilljs.com/docs/api/#getlength
getLength
    :: forall eff
     . Editor
    -> Eff (quill :: QUILL | eff) (Return Int)
getLength editor =
    runExcept <<< readInt <$> runFn1 getLengthImpl
        editor

foreign import getLengthImpl
    :: forall eff
     . Fn1
        Editor -- self
        (Eff (quill :: QUILL | eff) Foreign)

-- | https://quilljs.com/docs/api/#gettext
getText
    :: forall eff
     . Int
    -> DefaultArg Int
    -> Editor
    -> Eff (quill :: QUILL | eff) (Return String)
getText index length editor =
    runExcept <<< readString <$> runFn3 getTextImpl
        editor
        index
        (fromMaybe (unsafeCoerce undefined) length)

foreign import getTextImpl
    :: forall eff
     . Fn3
        Editor -- self
        Int    -- index
        Int    -- length
        (Eff (quill :: QUILL | eff) Foreign)

-- | https://quilljs.com/docs/api/#insertembed
insertEmbed
    :: forall eff
     . Int
    -> Embed
    -> Source
    -> Editor
    -> Eff (quill :: QUILL | eff) (Return Ops)
insertEmbed index (Image url) source self =
    runExcept <<< readOps <$> runFn5 insertEmbedImpl
        self
        index
        "image"
        url
        (show source)
insertEmbed index (Video url) source self =
    runExcept <<< readOps <$> runFn5 insertEmbedImpl
        self
        index
        "video"
        url
        (show source)

foreign import insertEmbedImpl
    :: forall eff
     . Fn5
        Editor -- self
        Int    -- index
        String -- type
        String -- value
        String -- source
        (Eff (quill :: QUILL | eff) Foreign)

-- | https://quilljs.com/docs/api/#inserttext
insertText
    :: forall eff
     . Int
    -> String
    -> Options Formats
    -> Source
    -> Editor
    -> Eff (quill :: QUILL | eff) (Return Ops)
insertText index text formats source self =
    runExcept <<< readOps <$> runFn5 insertTextImpl
        self
        index
        text
        (options formats)
        (show source)

foreign import insertTextImpl
    :: forall eff
     . Fn5
        Editor  -- self
        Int     -- index
        String  -- text
        Foreign -- formats
        String  -- source
        (Eff (quill :: QUILL | eff) Foreign)

-- | https://quilljs.com/docs/api/#setcontents
setContents
    :: forall eff
     . Ops
    -> DefaultArg Source
    -> Editor
    -> Eff (quill :: QUILL | eff) (Return Ops)
setContents ops source self =
    runExcept <<< readOps <$> runFn3 setContentsImpl
        self
        (opsToForeign ops)
        (fromMaybe Source.API source # show)

foreign import setContentsImpl
    :: forall eff
     . Fn3
        Editor  -- self
        Foreign -- delta
        String  -- source
        (Eff (quill :: QUILL | eff) Foreign)

-- | https://quilljs.com/docs/api/#settext
setText
    :: forall eff
     . String
    -> DefaultArg Source
    -> Editor
    -> Eff (quill :: QUILL | eff) (Return Ops)
setText text source self =
    runExcept <<< readOps <$> runFn3 setTextImpl
        self
        text
        (fromMaybe Source.API source # show)

foreign import setTextImpl
    :: forall eff
     . Fn3
        Editor -- self
        String -- text
        String -- source
        (Eff (quill :: QUILL | eff) Foreign)

-- | https://quilljs.com/docs/api/#updatecontents
updateContents
    :: forall eff
     . Ops
    -> DefaultArg Source
    -> Editor
    -> Eff (quill :: QUILL | eff) (Return Ops)
updateContents ops source self =
    runExcept <<< readOps <$> runFn3 updateContentsImpl
        self
        (opsToForeign ops)
        (fromMaybe Source.API source # show)

foreign import updateContentsImpl
    :: forall eff
     . Fn3
        Editor  -- self
        Foreign -- delta
        String  -- source
        (Eff (quill :: QUILL | eff) Foreign)

foreign import undefined :: Foreign -- for optional arguments
