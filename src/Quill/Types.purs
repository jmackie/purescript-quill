module Quill.Types where

import Control.Monad.Eff (kind Effect)

data Quill

foreign import data QUILL :: Effect
