module Quill.API.Source
    ( Source(User, API, Silent)
    )
where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.List.NonEmpty as NonEmptyList
import Foreign (Foreign, ForeignError(ForeignError), F)
import Foreign (readString) as Foreign
import Foreign.Class (class Decode) as Foreign


-- | NOTE: Calls where the source is "user" when the editor is disabled are ignored.
data Source
    = User
    | API
    | Silent

instance showSource :: Show Source where
    show User   = "user"
    show API    = "api"
    show Silent = "silent"

instance foreignDecodeSource :: Foreign.Decode Source where decode = decodeSource


decodeSource :: Foreign -> F Source
decodeSource = Foreign.readString >=> case _ of
    "user"   -> pure User
    "api"    -> pure API
    "silent" -> pure Silent
    other    -> throwError <<< NonEmptyList.singleton <<< ForeignError $
                "unrecognised source: " <> other
