module Quill.API.Editor
    ( blur
    , disable
    , enable
    , focus
    , hasFocus
    , update
    ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Except (runExcept)

import Data.Foreign (Foreign, readBoolean)
import Data.Function.Uncurried (Fn1, runFn1, Fn2, runFn2)
import Data.Maybe (Maybe, fromMaybe)

import Quill (Editor)
import Quill.API.Return (Return)
import Quill.API.Source (Source)
import Quill.API.Source as Source
import Quill.Types (QUILL)

--------------------------------------------------------------------------------
-- | https://quilljs.com/docs/api/#blur
blur
    :: forall eff
     . Editor
    -> Eff (quill :: QUILL | eff) Unit
blur editor =
    void $ runFn1 blurImpl
        editor

foreign import blurImpl
    :: forall eff
     . Fn1
        Editor -- self
        (Eff (quill :: QUILL | eff) Unit)

--------------------------------------------------------------------------------
-- | https://quilljs.com/docs/api/#disable
disable
    :: forall eff
     . Editor
    -> Eff (quill :: QUILL | eff) Unit
disable editor =
    void $ runFn1 disableImpl
        editor

foreign import disableImpl
    :: forall eff
     . Fn1
        Editor -- self
        (Eff (quill :: QUILL | eff) Unit)

--------------------------------------------------------------------------------
-- | https://quilljs.com/docs/api/#enable
enable
    :: forall eff
     . Boolean
    -> Editor
    -> Eff (quill :: QUILL | eff) Unit
enable enabled editor =
    void $ runFn2 enableImpl
        editor
        enabled

foreign import enableImpl
    :: forall eff
     . Fn2
        Editor  -- self
        Boolean -- enabled
        (Eff (quill :: QUILL | eff) Unit)

--------------------------------------------------------------------------------
-- | https://quilljs.com/docs/api/#focus
focus
    :: forall eff
     . Editor
    -> Eff (quill :: QUILL | eff) Unit
focus editor =
    void $ runFn1 focusImpl
        editor

foreign import focusImpl
    :: forall eff
     . Fn1
        Editor -- self
        (Eff (quill :: QUILL | eff) Unit)

--------------------------------------------------------------------------------
-- | https://quilljs.com/docs/api/#hasfocus
hasFocus
    :: forall eff
     . Editor
    -> Eff (quill :: QUILL | eff) (Return Boolean)
hasFocus editor =
    runExcept <<< readBoolean <$> runFn1 hasFocusImpl
        editor

foreign import hasFocusImpl
    :: forall eff
     . Fn1
        Editor -- self
        (Eff (quill :: QUILL | eff) Foreign)


--------------------------------------------------------------------------------
-- | https://quilljs.com/docs/api/#update
update
    :: forall eff
     . DefaultArg Source
    -> Editor
    -> Eff (quill :: QUILL | eff) Unit
update source editor =
    void $ runFn2 updateImpl
        editor
        (fromMaybe Source.API source # show)

foreign import updateImpl
    :: forall eff
     . Fn2
        Editor -- self
        String -- source
        (Eff (quill :: QUILL | eff) Unit)

--------------------------------------------------------------------------------

type DefaultArg = Maybe

