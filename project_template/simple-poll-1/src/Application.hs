{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Data.IORef
import Control.Lens
import Snap.Snaplet

import qualified Data.Map as M
import qualified Data.ByteString as B

------------------------------------------------------------------------------
type MapRef = IORef (M.Map B.ByteString Int)

data App = App
    { _mapRef :: MapRef
    }

makeLenses ''App

------------------------------------------------------------------------------
type AppHandler = Handler App App
