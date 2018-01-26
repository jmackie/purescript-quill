module Quill.Types
    ( QUILL
    , Quill
    , null
    ) where

import Control.Monad.Eff (kind Effect)

import Data.Foreign (Foreign)

-- | The `Quill` effect type.
foreign import data QUILL :: Effect

-- | Quill is the object type exported by `quilljs`. If available it
-- | will be re-exported by this package, otherwise a reference error
-- | will be thrown on initialisation.
data Quill

-- | Javascript null.
foreign import null :: Foreign
