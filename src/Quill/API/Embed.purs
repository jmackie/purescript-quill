module Quill.API.Embed
    ( Embed(..)
    , URL
    ) where

-- | Embed an external resource.
data Embed
    = Image URL
    | Video URL

-- | For clarity.
type URL = String

