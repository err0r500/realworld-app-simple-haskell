{-# LANGUAGE RankNTypes         #-}

module Adapter.Http.Lib where

import           RIO

import qualified Usecase.Interactor            as UC
import qualified Usecase.LogicHandler          as UC
import qualified Network.Wai                   as Wai

type Router m
  =  (MonadUnliftIO m, MonadIO m, UC.Logger m)
  => UC.LogicHandler m
  -> (forall a. m a -> IO a)
  -> IO Wai.Application
