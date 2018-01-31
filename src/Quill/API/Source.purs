module Quill.API.Source
    ( Source(..)
    , readSource
    ) where

import Prelude

import Control.Monad.Except.Trans (except)

import Data.Either (Either(..))
import Data.Foreign (Foreign, ForeignError(..), F, readString)
import Data.List.NonEmpty (singleton)

-- | NOTE: Calls where the source is "user" when the editor is disabled are ignored.
data Source
    = User
    | API
    | Silent

instance showSource :: Show Source where
    show User   = "user"
    show API    = "api"
    show Silent = "silent"

readSource :: Foreign -> F Source
readSource value =
    readString value >>= (case _ of
        "user"   -> pure User
        "api"    -> pure API
        "silent" -> pure Silent
        nope -> except $ Left $ singleton $
            ForeignError ("unrecognised source: " <> nope))
