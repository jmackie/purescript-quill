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
import Control.Monad.Eff.Class (liftEff)

import Data.Foreign (Foreign, readBoolean)
import Data.Function.Uncurried (Fn1, runFn1, Fn2, runFn2)
import Data.Maybe (Maybe)

import Quill (Editor)
import Quill.API.API (API, handleReturn)
import Quill.API.Source (Source)
import Quill.Types (QUILL)

--------------------------------------------------------------------------------
-- | https://quilljs.com/docs/api/#blur
blur
    :: forall eff
     . Editor
    -> API (quill :: QUILL | eff) Unit
blur editor =
    void <<< liftEff $
        runFn1 blurImpl
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
    -> API (quill :: QUILL | eff) Unit
disable editor =
    void <<< liftEff $
        runFn1 disableImpl
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
    -> API (quill :: QUILL | eff) Unit
enable enabled editor =
    void <<< liftEff $
        runFn2 enableImpl
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
    -> API (quill :: QUILL | eff) Unit
focus editor =
    void <<< liftEff $
        runFn1 focusImpl
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
    -> API (quill :: QUILL | eff) Boolean
hasFocus editor =
    handleReturn readBoolean <=< liftEff $
        runFn1 hasFocusImpl
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
     . Source
    -> Editor
    -> Eff (quill :: QUILL | eff) Unit
update source editor =
    void <<< liftEff $
        runFn2 updateImpl
            editor
            (show source)

foreign import updateImpl
    :: forall eff
     . Fn2
        Editor -- self
        String -- source
        (Eff (quill :: QUILL | eff) Unit)

--------------------------------------------------------------------------------

type DefaultArg = Maybe

