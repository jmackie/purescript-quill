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
import Control.Monad.Eff.Class (liftEff)

import Data.Function.Uncurried (Fn1, runFn1, Fn3, runFn3,
                                Fn4, runFn4, Fn5, runFn5)

import Data.Foreign (Foreign, readString, readInt)
import Data.Maybe (Maybe, fromMaybe)
import Data.Options (Options, options)

import Quill (Editor)
import Quill.API.API (API, handleReturn)
import Quill.API.Delta (Ops, readOps, opsToForeign)
import Quill.API.Embed (Embed(..))
import Quill.API.Formats (Formats)
import Quill.API.Range (Range, Index, Length, index, length)
import Quill.API.Source (Source)
import Quill.API.Source as Source
import Quill.Types (QUILL)

import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------
-- | https://quilljs.com/docs/api/#deletetext
deleteText
    :: forall eff
     . Range
    -> DefaultArg Source
    -> Editor
    -> API (quill :: QUILL | eff) Ops
deleteText range source editor =
    handleReturn readOps <=< liftEff $
        runFn4 deleteTextImpl
            editor
            (index range)
            (length range)
            (fromMaybe Source.API source # show)

foreign import deleteTextImpl
    :: forall eff
     . Fn4
        Editor -- self
        Index  -- index
        Length -- length
        String -- source
        (Eff (quill :: QUILL | eff) Foreign)

--------------------------------------------------------------------------------
-- | https://quilljs.com/docs/api/#getcontents
getContents
    :: forall eff
     . Index
    -> DefaultArg Length
    -> Editor
    -> API (quill :: QUILL | eff) Ops
getContents index length editor =
    handleReturn readOps <=< liftEff $
        runFn3 getContentsImpl
            editor
            index
            (fromMaybe (unsafeCoerce undefined) length)

foreign import getContentsImpl
    :: forall eff
     . Fn3
        Editor -- self
        Index  -- index
        Length -- length
        (Eff (quill :: QUILL | eff) Foreign)

--------------------------------------------------------------------------------
-- | https://quilljs.com/docs/api/#getlength
getLength
    :: forall eff
     . Editor
    -> API (quill :: QUILL | eff) Int
getLength editor =
    handleReturn readInt <=< liftEff $
        runFn1 getLengthImpl
            editor

foreign import getLengthImpl
    :: forall eff
     . Fn1
        Editor -- self
        (Eff (quill :: QUILL | eff) Foreign)

--------------------------------------------------------------------------------
-- | https://quilljs.com/docs/api/#gettext
getText
    :: forall eff
     . Index
    -> DefaultArg Length
    -> Editor
    -> API (quill :: QUILL | eff) String
getText index length editor =
    handleReturn readString <=< liftEff $
        runFn3 getTextImpl
            editor
            index
            (fromMaybe (unsafeCoerce undefined) length)

foreign import getTextImpl
    :: forall eff
     . Fn3
        Editor -- self
        Index  -- index
        Length -- length
        (Eff (quill :: QUILL | eff) Foreign)

--------------------------------------------------------------------------------
-- | https://quilljs.com/docs/api/#insertembed
insertEmbed
    :: forall eff
     . Index
    -> Embed
    -> Source
    -> Editor
    -> API (quill :: QUILL | eff) Ops

insertEmbed index (Image url) source self =
    handleReturn readOps <=< liftEff $
        runFn5 insertEmbedImpl
            self
            index
            "image"
            url
            (show source)

insertEmbed index (Video url) source self =
    handleReturn readOps <=< liftEff $
        runFn5 insertEmbedImpl
            self
            index
            "video"
            url
            (show source)

foreign import insertEmbedImpl
    :: forall eff
     . Fn5
        Editor -- self
        Index  -- index
        String -- type
        String -- value
        String -- source
        (Eff (quill :: QUILL | eff) Foreign)

--------------------------------------------------------------------------------
-- | https://quilljs.com/docs/api/#inserttext
insertText
    :: forall eff
     . Index
    -> String
    -> Options Formats
    -> Source
    -> Editor
    -> API (quill :: QUILL | eff) Ops
insertText index text formats source self =
    handleReturn readOps <=< liftEff $
        runFn5 insertTextImpl
            self
            index
            text
            (options formats)
            (show source)

foreign import insertTextImpl
    :: forall eff
     . Fn5
        Editor  -- self
        Index   -- index
        String  -- text
        Foreign -- formats
        String  -- source
        (Eff (quill :: QUILL | eff) Foreign)

--------------------------------------------------------------------------------
-- | https://quilljs.com/docs/api/#setcontents
setContents
    :: forall eff
     . Ops
    -> DefaultArg Source
    -> Editor
    -> API (quill :: QUILL | eff) Ops
setContents ops source self =
    handleReturn readOps <=< liftEff $
        runFn3 setContentsImpl
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

--------------------------------------------------------------------------------
-- | https://quilljs.com/docs/api/#settext
setText
    :: forall eff
     . String
    -> DefaultArg Source
    -> Editor
    -> API (quill :: QUILL | eff) Ops
setText text source self =
    handleReturn readOps <=< liftEff $
        runFn3 setTextImpl
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

--------------------------------------------------------------------------------
-- | https://quilljs.com/docs/api/#updatecontents
updateContents
    :: forall eff
     . Ops
    -> DefaultArg Source
    -> Editor
    -> API (quill :: QUILL | eff) Ops
updateContents ops source self =
    handleReturn readOps <=< liftEff $
        runFn3 updateContentsImpl
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

--------------------------------------------------------------------------------

type DefaultArg = Maybe

foreign import undefined :: Foreign -- for optional arguments

