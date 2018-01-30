module Quill.API.Source (Source(..)) where

import Prelude

-- | NOTE: Calls where the source is "user" when the editor is disabled are ignored.
data Source
    = User
    | API
    | Silent

instance showSource :: Show Source where
    show User   = "user"
    show API    = "api"
    show Silent = "silent"
