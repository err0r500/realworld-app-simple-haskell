module Usecase.LogicHandler where

import qualified Usecase.UserRegistration      as UC
import qualified Usecase.UserLogin             as UC

data LogicHandler m = LogicHandler {
  userRegister_ :: UC.Register m,
  userLogin_ :: UC.Login m
}
