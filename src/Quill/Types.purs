module Quill.Types
    ( QUILL
    ) where

import Control.Monad.Eff (kind Effect)

-- | The `Quill` effect type.
foreign import data QUILL :: Effect

