module Quill.API.Content
    ( deleteText
    , getContents
    ) where

import Prelude

import Control.Monad.Eff (Eff)

import Data.Foreign (F, Foreign)
import Data.Function.Uncurried (Fn3, runFn3, Fn4, runFn4)

import Quill (Editor)
import Quill.API.Delta (Delta, readDelta)
import Quill.API.Source (Source, sourceToForeign)
import Quill.Types (QUILL)

-- | https://quilljs.com/docs/api/#deletetext
deleteText
    :: forall eff
     .Number -> Number -> Source -> Editor -> Eff (quill :: QUILL | eff) (F Delta)
deleteText index length source editor = do
    readDelta <$> runFn4 deleteTextImpl editor index length (sourceToForeign source)

foreign import deleteTextImpl
    :: forall eff
     . Fn4 Editor Number Number Foreign (Eff (quill :: QUILL | eff) Foreign)

-- | https://quilljs.com/docs/api/#getcontents
getContents
    :: forall eff
     .Number -> Number -> Editor -> Eff (quill :: QUILL | eff) (F Delta)
getContents index length editor =
    readDelta <$> runFn3 getContentsImpl editor index length

foreign import getContentsImpl
    :: forall eff
     . Fn3 Editor Number Number (Eff (quill :: QUILL | eff) Foreign)

