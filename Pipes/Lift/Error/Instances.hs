{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pipes.Lift.Error.Instances where

import Pipes (MFunctor, hoist)
import Control.Error (EitherT(..), EitherRT(..), runEitherT)

instance MFunctor (EitherT e) where
    hoist nat m = EitherT (nat (runEitherT m))

instance MFunctor (EitherRT r) where
    hoist nat m = EitherRT (hoist nat (runEitherRT m))
