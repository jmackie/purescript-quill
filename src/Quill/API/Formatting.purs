module Quill.API.Formatting
    ( format
    , formatLine
    , formatText
    , removeFormat
    ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Except (runExcept)

import Data.Foreign (Foreign)
import Data.Function.Uncurried (Fn4, runFn4, Fn5, runFn5)
import Data.Maybe (Maybe, fromMaybe)
import Data.Options (Options, options)

import Quill (Editor)
import Quill.API.Delta (Ops, readOps)
import Quill.API.Formats (Formats, SingleFormat, name, value)
import Quill.API.Range (Range, Index, index, Length, length)
import Quill.API.Return (Return)
import Quill.API.Source (Source)
import Quill.API.Source as Source
import Quill.Types (QUILL)

--------------------------------------------------------------------------------
-- | https://quilljs.com/docs/api/#format
format
    :: forall eff
     . SingleFormat
    -> DefaultArg Source
    -> Editor
    -> Eff (quill :: QUILL | eff) (Return Ops)
format fmt source editor =
    runExcept <<< readOps <$> runFn4 formatImpl
        editor
        (name fmt)
        (value fmt)
        (fromMaybe Source.API source # show)

foreign import formatImpl
    :: forall eff
     . Fn4
        Editor  -- self
        String  -- name
        Foreign -- value
        String  -- source
        (Eff (quill :: QUILL | eff) Foreign)

--------------------------------------------------------------------------------
-- | https://quilljs.com/docs/api/#formatline
formatLine
    :: forall eff
     . Range
    -> Options Formats
    -> DefaultArg Source
    -> Editor
    -> Eff (quill :: QUILL | eff) (Return Ops)
formatLine range formats source editor =
    runExcept <<< readOps <$> runFn5 formatLineImpl
        editor
        (index range)
        (length range)
        (options formats)
        (fromMaybe Source.API source # show)

foreign import formatLineImpl
    :: forall eff
     . Fn5
        Editor  -- self
        Index   -- index
        Length  -- length
        Foreign -- formats
        String  -- source
        (Eff (quill :: QUILL | eff) Foreign)

--------------------------------------------------------------------------------
-- | https://quilljs.com/docs/api/#formattext
formatText
    :: forall eff
     . Range
    -> Options Formats
    -> DefaultArg Source
    -> Editor
    -> Eff (quill :: QUILL | eff) (Return Ops)
formatText range formats source editor =
    runExcept <<< readOps <$> runFn5 formatTextImpl
        editor
        (index range)
        (length range)
        (options formats)
        (fromMaybe Source.API source # show)

foreign import formatTextImpl
    :: forall eff
     . Fn5
        Editor  -- self
        Index   -- index
        Length  -- length
        Foreign -- formats
        String  -- source
        (Eff (quill :: QUILL | eff) Foreign)

--------------------------------------------------------------------------------
-- | https://quilljs.com/docs/api/#removeformat
removeFormat
    :: forall eff
     . Range
    -> DefaultArg Source
    -> Editor
    -> Eff (quill :: QUILL | eff) (Return Ops)
removeFormat range source editor =
    runExcept <<< readOps <$> runFn4 removeFormatImpl
        editor
        (index range)
        (length range)
        (fromMaybe Source.API source # show)

foreign import removeFormatImpl
    :: forall eff
     . Fn4
        Editor  -- self
        Index   -- index
        Length  -- length
        String  -- source
        (Eff (quill :: QUILL | eff) Foreign)

--------------------------------------------------------------------------------

type DefaultArg = Maybe

