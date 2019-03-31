module Adapter.Http.Shared where

import           ClassyPrelude
import qualified Usecase.Class as UC

type AppMonadicStack m = (MonadIO m, UC.UserRepo m, UC.Logger m, UC.UUIDGen m, UC.EmailChecker m)
