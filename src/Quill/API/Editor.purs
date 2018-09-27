module Quill.API.Editor
    ( blur
    , disable
    , enable
    , focus
    , hasFocus
    , update
    )
where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Except (runExcept)
import Data.Either (either)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried as UncurriedEffect
import Foreign (Foreign)
import Foreign (MultipleErrors, readBoolean) as Foreign
import Quill.API.Source (Source)
import Quill.Editor (Editor)


-- | https://quilljs.com/docs/api/#blur
blur :: forall m. MonadEffect m => Editor -> m Unit
blur editor = liftEffect (UncurriedEffect.runEffectFn1 blurImpl editor)

foreign import blurImpl :: UncurriedEffect.EffectFn1 Editor Unit


-- | https://quilljs.com/docs/api/#disable
disable :: forall m. MonadEffect m => Editor -> m Unit
disable editor = liftEffect (UncurriedEffect.runEffectFn1 disableImpl editor)

foreign import disableImpl :: UncurriedEffect.EffectFn1 Editor Unit


-- | https://quilljs.com/docs/api/#enable
enable :: forall m. MonadEffect m => Boolean -> Editor -> m Unit
enable enabled editor =
    liftEffect (UncurriedEffect.runEffectFn2 enableImpl editor enabled)

foreign import enableImpl :: UncurriedEffect.EffectFn2 Editor Boolean Unit


-- | https://quilljs.com/docs/api/#focus
focus :: forall m. MonadEffect m => Editor -> m Unit
focus editor = liftEffect (UncurriedEffect.runEffectFn1 focusImpl editor)

foreign import focusImpl :: UncurriedEffect.EffectFn1 Editor Unit


-- | https://quilljs.com/docs/api/#hasfocus
hasFocus
    :: forall m
     . MonadEffect m
    => MonadError Foreign.MultipleErrors m
    => Editor
    -> m Boolean
hasFocus editor =
    either throwError pure <<< runExcept <<< Foreign.readBoolean <=< liftEffect $
    UncurriedEffect.runEffectFn1 hasFocusImpl
        editor

foreign import hasFocusImpl :: UncurriedEffect.EffectFn1 Editor Foreign


-- | https://quilljs.com/docs/api/#update
update :: forall m. MonadEffect m => Source -> Editor -> m Unit
update source editor =
    liftEffect (UncurriedEffect.runEffectFn2 updateImpl editor (show source))

foreign import updateImpl :: UncurriedEffect.EffectFn2 Editor String Unit
