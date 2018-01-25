module Quill.API.Source
    ( Source(..)
    , sourceToForeign
    ) where

import Prelude

import Data.Foreign (Foreign, toForeign)

-- | Source may be "user", "api", or "silent". Calls where the source is "user"
-- | when the editor is disabled are ignored.
data Source
    = User
    | API
    | Silent

-- | Convert `Source` to it's javascript representation.
sourceToForeign :: Source -> Foreign
sourceToForeign = toForeign <<< case _ of
    User   -> "user"
    API    -> "api"
    Silent -> "silent"
