module Quill.API.Formatting
    ( format
    , formatLine
    , formatText
    , removeFormat
    ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Identity (Identity)
import Data.Newtype (unwrap)
import Data.Options (Options, options)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried as UncurriedEffect
import Foreign (Foreign)
import Foreign (MultipleErrors) as Foreign
import Foreign.Class (decode) as Foreign
import Quill.API.Delta (Ops)
import Quill.API.Formats (Formats, SingleFormat)
import Quill.API.Formats as Formats
import Quill.API.Range (Range)
import Quill.API.Source (Source)
import Quill.Editor (Editor)


-- | https://quilljs.com/docs/api/#format
format
    :: forall m
     . MonadEffect m
    => MonadError Foreign.MultipleErrors m
    => SingleFormat
    -> Source
    -> Editor
    -> m Ops
format fmt source editor =
    either throwError pure <<< runExcept <<< Foreign.decode <=< liftEffect $
    UncurriedEffect.runEffectFn4 formatImpl
        editor (Formats.name fmt) (Formats.value fmt) (show source)

foreign import formatImpl
    :: UncurriedEffect.EffectFn4
            Editor
            String  -- name
            Foreign -- value
            String  -- source
            Foreign


-- | https://quilljs.com/docs/api/#formatline
formatLine
    :: forall m
     . MonadEffect m
    => MonadError Foreign.MultipleErrors m
    => Range Identity
    -> Options Formats
    -> Source
    -> Editor
    -> m Ops
formatLine { index, length } formats source editor =
    either throwError pure <<< runExcept <<< Foreign.decode <=< liftEffect $
    UncurriedEffect.runEffectFn5 formatLineImpl
        editor index (unwrap length ) (options formats) (show source)

foreign import formatLineImpl
    :: UncurriedEffect.EffectFn5
            Editor
            Int     -- index
            Int     -- length
            Foreign -- formats
            String  -- source
            Foreign


-- | https://quilljs.com/docs/api/#formattext
formatText
    :: forall m
     . MonadEffect m
    => MonadError Foreign.MultipleErrors m
    => Range Identity
    -> Options Formats
    -> Source
    -> Editor
    -> m Ops
formatText { index, length } formats source editor =
    either throwError pure <<< runExcept <<< Foreign.decode <=< liftEffect $
    UncurriedEffect.runEffectFn5 formatTextImpl
        editor index (unwrap length) (options formats) (show source)

foreign import formatTextImpl
    :: UncurriedEffect.EffectFn5
            Editor
            Int     -- index
            Int     -- length
            Foreign -- formats
            String  -- source
            Foreign


-- | https://quilljs.com/docs/api/#removeformat
removeFormat
    :: forall m
     . MonadEffect m
    => MonadError Foreign.MultipleErrors m
    => Range Identity
    -> Source
    -> Editor
    -> m Ops
removeFormat { index, length } source editor =
    either throwError pure <<< runExcept <<< Foreign.decode <=< liftEffect $
    UncurriedEffect.runEffectFn4 removeFormatImpl
        editor index (unwrap length) (show source)

foreign import removeFormatImpl
    :: UncurriedEffect.EffectFn4
            Editor
            Int     -- index
            Int     -- length
            String  -- source
            Foreign
