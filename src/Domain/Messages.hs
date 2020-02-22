{-# LANGUAGE GADTs #-}

module Domain.Messages where

import           ClassyPrelude

data Message a where
     ErrorMsg ::Show a => a -> Message a
     WarningMsg ::Show a => a -> Message a
     InfoMsg ::Show a => a -> Message a
     DebugMsg ::Show a => a -> Message a


instance Show (Message a) where
        show (ErrorMsg   a) = show a
        show (WarningMsg a) = show a
        show (InfoMsg    a) = show a
        show (DebugMsg   a) = show a
