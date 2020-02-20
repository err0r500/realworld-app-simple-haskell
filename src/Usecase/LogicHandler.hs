module Usecase.LogicHandler where

import           ClassyPrelude
import           Domain.User                   as D
import qualified Usecase.UserRegistration      as UC

data LogicHandler m = LogicHandler {
  register_ :: UC.Register m
}
