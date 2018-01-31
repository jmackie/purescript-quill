module Quill.API.API
    ( API
    , handleReturn
    , runAPI
    ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (ExceptT, runExceptT, except)

import Data.Either (Either)
import Data.Foreign (Foreign, F, MultipleErrors)

-- | API calls are effectful, and may also fail in handling the returned value.
type API eff a = ExceptT MultipleErrors (Eff eff) a

handleReturn :: forall a eff. (Foreign -> F a) -> Foreign -> API eff a
handleReturn f value = (runExcept >>> except) $ f value

runAPI :: forall a eff. API eff a -> Eff eff (Either MultipleErrors a)
runAPI = runExceptT

