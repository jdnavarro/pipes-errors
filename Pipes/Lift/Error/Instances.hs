{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pipes.Lift.Error.Instances where

import Pipes (MFunctor, hoist)
import Control.Error (ExceptRT(ExceptRT, runExceptRT))

instance MFunctor (ExceptRT r) where
    hoist nat m = ExceptRT (hoist nat (runExceptRT m))
