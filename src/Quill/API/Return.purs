module Quill.API.Return (Return) where

import Data.Either (Either)
import Data.Foreign (MultipleErrors)

-- | Values returned by the Quill API may not type check.
type Return a = Either MultipleErrors a
