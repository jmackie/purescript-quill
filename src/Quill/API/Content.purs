module Quill.API.Content
    ( deleteText
    , getContents
    , getLength
    , getText
    ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Except (runExcept)

import Data.Foreign (Foreign, toForeign,
                     readString, readInt)
import Data.Function.Uncurried (Fn1, runFn1, Fn3, runFn3, Fn4, runFn4)
import Data.Maybe (Maybe, maybe)

import Quill (Editor)
import Quill.API.Delta (Deltas, readDeltas)
import Quill.API.Return (Return)
import Quill.API.Source (Source, sourceToForeign)
import Quill.Types (QUILL, null)

-- | https://quilljs.com/docs/api/#deletetext
-- | NOTE: I think this might actually return an array of Deltass...
deleteText
    :: forall eff
     . Number -> Number -> Source -> Editor -> Eff (quill :: QUILL | eff) (Return Deltas)
deleteText index length source editor = runExcept <<< readDeltas <$> runFn4 deleteTextImpl
    editor index length (sourceToForeign source)

foreign import deleteTextImpl
    :: forall eff
     . Fn4 Editor Number Number Foreign (Eff (quill :: QUILL | eff) Foreign)

-- | https://quilljs.com/docs/api/#getcontents
getContents
    :: forall eff
     . Number -> (Maybe Number) -> Editor -> Eff (quill :: QUILL | eff) (Return Deltas)
getContents index length editor = runExcept <<< readDeltas <$> runFn3 getContentsImpl
    editor index (maybe null toForeign length)

foreign import getContentsImpl
    :: forall eff
     . Fn3 Editor Number Foreign (Eff (quill :: QUILL | eff) Foreign)

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
     . Number -> (Maybe Number) -> Editor -> Eff (quill :: QUILL | eff) (Return String)
getText index length editor = runExcept <<< readString <$> runFn3 getTextImpl
    editor index (maybe null toForeign length)

foreign import getTextImpl
    :: forall eff
     . Fn3 Editor Number Foreign (Eff (quill :: QUILL | eff) Foreign)

