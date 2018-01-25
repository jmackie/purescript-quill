module Quill.Types where

import Control.Monad.Eff (kind Effect)

-- | Quill is the object type exported by `quilljs`. If available it
-- | will be re-exported by this package, otherwise a reference error
-- | will be thrown on initialisation.
data Quill

-- | An instance of the `Quill` object.
data Editor = Editor

foreign import data QUILL :: Effect
