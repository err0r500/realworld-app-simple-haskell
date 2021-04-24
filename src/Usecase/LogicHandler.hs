module Usecase.LogicHandler where

import qualified Usecase.UserLogin as UC
import qualified Usecase.UserRegistration as UC

data LogicHandler m = LogicHandler
  { _userRegister :: UC.Register m,
    _userLogin :: UC.Login m
  }
