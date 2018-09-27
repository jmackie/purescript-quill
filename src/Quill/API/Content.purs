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
    )
where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Identity (Identity)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Options (Options, options)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried as UncurriedEffect
import Foreign (Foreign)
import Foreign (MultipleErrors, readInt, readString) as Foreign
import Foreign.Class (encode, decode) as Foreign
import Quill.API.Delta (Ops)
import Quill.API.Embed (Embed(Image, Video))
import Quill.API.Formats (Formats)
import Quill.API.Range (Range)
import Quill.API.Source (Source)
import Quill.Editor (Editor)


-- | https://quilljs.com/docs/api/#deletetext
deleteText
    :: forall m
     . MonadEffect m
    => MonadError Foreign.MultipleErrors m
    => Range Identity
    -> Source
    -> Editor
    -> m Ops
deleteText { index, length } source editor =
    either throwError pure <<< runExcept <<< Foreign.decode <=< liftEffect $
    UncurriedEffect.runEffectFn4 deleteTextImpl
        editor index (unwrap length) (show source)

foreign import deleteTextImpl
    :: UncurriedEffect.EffectFn4
            Editor
            Int    -- index
            Int    -- length
            String -- source
            Foreign


-- | https://quilljs.com/docs/api/#getcontents
getContents
    :: forall m
     . MonadEffect m
    => MonadError Foreign.MultipleErrors m
    => Range Maybe
    -> Editor
    -> m Ops
getContents { index, length } editor =
    either throwError pure <<< runExcept <<< Foreign.decode <=< liftEffect $
    UncurriedEffect.runEffectFn3 getContentsImpl
        editor index (Foreign.encode length)

foreign import getContentsImpl
    :: UncurriedEffect.EffectFn3
            Editor
            Int     -- index
            Foreign -- length
            Foreign


-- | https://quilljs.com/docs/api/#getlength
getLength
    :: forall m
     . MonadEffect m
    => MonadError Foreign.MultipleErrors m
    => Editor
    -> m Int
getLength editor =
    either throwError pure <<< runExcept <<< Foreign.readInt <=< liftEffect $
    UncurriedEffect.runEffectFn1 getLengthImpl
        editor

foreign import getLengthImpl
    :: UncurriedEffect.EffectFn1
            Editor
            Foreign


-- | https://quilljs.com/docs/api/#gettext
getText
    :: forall m
     . MonadEffect m
    => MonadError Foreign.MultipleErrors m
    => Range Maybe
    -> Editor
    -> m String
getText { index, length } editor =
    either throwError pure <<< runExcept <<< Foreign.readString <=< liftEffect $
    UncurriedEffect.runEffectFn3 getTextImpl
        editor index (Foreign.encode length)

foreign import getTextImpl
    :: UncurriedEffect.EffectFn3
            Editor
            Int     -- index
            Foreign -- length
            Foreign


-- | https://quilljs.com/docs/api/#insertembed
insertEmbed
    :: forall m
     . MonadEffect m
    => MonadError Foreign.MultipleErrors m
    => Int
    -> Embed
    -> Source
    -> Editor
    -> m Ops
insertEmbed index (Image url) source editor =
    either throwError pure <<< runExcept <<< Foreign.decode <=< liftEffect $
    UncurriedEffect.runEffectFn5 insertEmbedImpl
        editor index "image" url (show source)
insertEmbed index (Video url) source editor =
    either throwError pure <<< runExcept <<< Foreign.decode <=< liftEffect $
    UncurriedEffect.runEffectFn5 insertEmbedImpl
        editor index "video" url (show source)

foreign import insertEmbedImpl
    :: UncurriedEffect.EffectFn5
            Editor
            Int    -- index
            String -- type
            String -- value
            String -- source
            Foreign


-- | https://quilljs.com/docs/api/#inserttext
insertText
    :: forall m
     . MonadEffect m
    => MonadError Foreign.MultipleErrors m
    => Int
    -> String
    -> Options Formats
    -> Source
    -> Editor
    -> m Ops
insertText index text formats source editor =
    either throwError pure <<< runExcept <<< Foreign.decode <=< liftEffect $
    UncurriedEffect.runEffectFn5 insertTextImpl
        editor index text (options formats) (show source)

foreign import insertTextImpl
    :: UncurriedEffect.EffectFn5
            Editor
            Int     -- index
            String  -- text
            Foreign -- formats
            String  -- source
            Foreign


-- | https://quilljs.com/docs/api/#setcontents
setContents
    :: forall m
     . MonadEffect m
    => MonadError Foreign.MultipleErrors m
    => Ops
    -> Source
    -> Editor
    -> m Ops
setContents ops source editor =
    either throwError pure <<< runExcept <<< Foreign.decode <=< liftEffect $
    UncurriedEffect.runEffectFn3 setContentsImpl
        editor (Foreign.encode ops) (show source)

foreign import setContentsImpl
    :: UncurriedEffect.EffectFn3
            Editor
            Foreign -- delta
            String  -- source
            Foreign


-- | https://quilljs.com/docs/api/#settext
setText
    :: forall m
     . MonadEffect m
    => MonadError Foreign.MultipleErrors m
    => String
    -> Source
    -> Editor
    -> m Ops
setText text source editor =
    either throwError pure <<< runExcept <<< Foreign.decode <=< liftEffect $
    UncurriedEffect.runEffectFn3 setTextImpl
        editor text (show source)

foreign import setTextImpl
    :: UncurriedEffect.EffectFn3
            Editor
            String -- text
            String -- source
            Foreign


-- | https://quilljs.com/docs/api/#updatecontents
updateContents
    :: forall m
     . MonadEffect m
    => MonadError Foreign.MultipleErrors m
    => Ops
    -> Source
    -> Editor
    -> m Ops
updateContents ops source editor =
    either throwError pure <<< runExcept <<< Foreign.decode <=< liftEffect $
    UncurriedEffect.runEffectFn3 updateContentsImpl
        editor (Foreign.encode ops) (show source)

foreign import updateContentsImpl
    :: UncurriedEffect.EffectFn3
            Editor
            Foreign -- delta
            String  -- source
            Foreign
