module Quill.API.Formatting
    ( format
    , formatLine
    , formatText
    , removeFormat
    ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)

import Data.Foreign (Foreign)
import Data.Function.Uncurried (Fn4, runFn4, Fn5, runFn5)
import Data.Maybe (Maybe)
import Data.Options (Options, options)

import Quill (Editor)
import Quill.API.API (API, handleReturn)
import Quill.API.Delta (Ops, readOps)
import Quill.API.Formats (Formats, SingleFormat, name, value)
import Quill.API.Range (Range, Index, Length, index, length)
import Quill.API.Source (Source)
import Quill.Types (QUILL)

--------------------------------------------------------------------------------
-- | https://quilljs.com/docs/api/#format
format
    :: forall eff
     . SingleFormat
    -> Source
    -> Editor
    -> API (quill :: QUILL | eff) Ops
format fmt source editor =
    handleReturn readOps <=< liftEff $
        runFn4 formatImpl
            editor
            (name fmt)
            (value fmt)
            (show source)

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
    -> Source
    -> Editor
    -> API (quill :: QUILL | eff) Ops
formatLine range formats source editor =
    handleReturn readOps <=< liftEff $
        runFn5 formatLineImpl
            editor
            (index range)
            (length range)
            (options formats)
            (show source)

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
    -> Source
    -> Editor
    -> API (quill :: QUILL | eff) Ops
formatText range formats source editor =
    handleReturn readOps <=< liftEff $
        runFn5 formatTextImpl
            editor
            (index range)
            (length range)
            (options formats)
            (show source)

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
    -> Source
    -> Editor
    -> API (quill :: QUILL | eff) Ops
removeFormat range source editor =
    handleReturn readOps <=< liftEff $
        runFn4 removeFormatImpl
            editor
            (index range)
            (length range)
            (show source)

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

