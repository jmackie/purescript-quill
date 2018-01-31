module Quill.API.Selection
    ( getBounds
    , getSelection
    , setSelection
    ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Except (runExcept)

import Data.Foreign (Foreign, F, readNumber)
import Data.Foreign.Index ((!))
import Data.Function.Uncurried (Fn2, runFn2, Fn3, runFn3, Fn4, runFn4)
import Data.Maybe (Maybe, fromMaybe)

import Quill (Editor)
import Quill.API.Range (Range, Index, Length, readRange, index, length)
import Quill.API.Return (Return)
import Quill.API.Source (Source)
import Quill.API.Source as Source
import Quill.Types (QUILL)

--------------------------------------------------------------------------------
-- | https://quilljs.com/docs/api/#getbounds
getBounds
    :: forall eff
     . Index
    -> DefaultArg Length
    -> Editor
    -> Eff (quill :: QUILL | eff) (Return Bounds)
getBounds index length editor =
    runExcept <<< readBounds <$> runFn3 getBoundsImpl
        editor
        index
        (fromMaybe 0 length)
    where
        readBounds :: Foreign -> F Bounds
        readBounds value = { left:_, top:_, height:_, width:_ }
            <$> (value ! "left"   >>= readNumber)
            <*> (value ! "top"    >>= readNumber)
            <*> (value ! "height" >>= readNumber)
            <*> (value ! "width"  >>= readNumber)

foreign import getBoundsImpl
    :: forall eff
     . Fn3
        Editor -- self
        Index  -- index
        Length -- length
        (Eff (quill :: QUILL | eff) Foreign)

--------------------------------------------------------------------------------
-- | https://quilljs.com/docs/api/#getselection
getSelection
    :: forall eff
     . Boolean
    -> Editor
    -> Eff (quill :: QUILL | eff) (Return Range)
getSelection focus editor =
    runExcept <<< readRange <$> runFn2 getSelectionImpl
        editor
        focus

foreign import getSelectionImpl
    :: forall eff
     . Fn2
        Editor  -- self
        Boolean -- focus
        (Eff (quill :: QUILL | eff) Foreign)

--------------------------------------------------------------------------------
-- | https://quilljs.com/docs/api/#setselection
setSelection
    :: forall eff
     . Range
    -> DefaultArg Source
    -> Editor
    -> Eff (quill :: QUILL | eff) Unit
setSelection range source editor =
    void $ runFn4 setSelectionImpl
        editor
        (index range)
        (length range)
        (fromMaybe Source.API source # show)

foreign import setSelectionImpl
    :: forall eff
     . Fn4
        Editor -- self
        Index  -- index
        Length -- length
        String -- source
        (Eff (quill :: QUILL | eff) Unit)

--------------------------------------------------------------------------------

type DefaultArg = Maybe

type Bounds = { left   :: Number
              , top    :: Number
              , height :: Number
              , width  :: Number
              }

