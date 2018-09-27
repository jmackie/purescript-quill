module Quill.Editor
    ( Editor
    , new
    )
where

import Prelude

import Data.Options (Options, options)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Foreign (Foreign)
import Quill.Config (Config)
import Web.HTML.HTMLElement (HTMLElement)


-- | An instance of the `Quill` javascript object.
newtype Editor = Editor Foreign


-- | Initialise a Quill `Editor` on the given element.
new :: forall m. MonadEffect m => Options Config -> HTMLElement -> m Editor
new config element = liftEffect (Editor <$> runEffectFn2 newImpl element (options config))


foreign import newImpl :: EffectFn2 HTMLElement Foreign Foreign
