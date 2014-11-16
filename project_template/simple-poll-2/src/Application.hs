{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Data.IORef
import Control.Lens
import Snap.Snaplet
import Snap.Snaplet.Heist

import qualified Data.Map as M
import qualified Data.Text as T

------------------------------------------------------------------------------
type MapRef = IORef (M.Map T.Text Int)

data App = App
    { _heist :: Snaplet (Heist App)
    , _mapRef :: MapRef
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist


------------------------------------------------------------------------------
type AppHandler = Handler App App
