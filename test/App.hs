{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module App where

import qualified Adapter.EmailChecker      as RealEmailChecker
import qualified Adapter.InMemory.Logger   as InMemLogger
import qualified Adapter.InMemory.UserRepo as InMemUserRepo
import qualified Adapter.InMemory.UuidGen  as InMemUuidGen
import qualified Adapter.Logger            as Katip
import           ClassyPrelude
import           Usecase.Class

type UsersState = TVar InMemUserRepo.UsersState

type LoggerState = TVar InMemLogger.Logs

type UUIDGen_ = TVar InMemUuidGen.UUIDGen

newtype InMemoryApp a = InMemoryApp
    { unApp :: ReaderT (UsersState, LoggerState, UUIDGen_) IO a
    } deriving (Applicative, Functor, Monad, MonadReader (UsersState, LoggerState, UUIDGen_), MonadIO)

run :: (UsersState, LoggerState, UUIDGen_) -> InMemoryApp a -> IO a
run (state, logger, uuid) app = runReaderT (unApp app) (state, logger, uuid)

instance Usecase.Class.UserRepo InMemoryApp where
    getUserByID = InMemUserRepo.getUserByID
    getUserByName = InMemUserRepo.getUserByName
    getUserByEmail = InMemUserRepo.getUserByEmail

instance Usecase.Class.Logger InMemoryApp where
    log = InMemLogger.log

instance Usecase.Class.UUIDGen InMemoryApp where
    genUUID = InMemUuidGen.genUUIDv4

instance Usecase.Class.EmailChecker InMemoryApp where
    checkEmailFormat = RealEmailChecker.checkEmailFormat
