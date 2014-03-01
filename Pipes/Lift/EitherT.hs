{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pipes.Lift.EitherT where

import Control.Monad ((>=>))
import Pipes (Proxy, MFunctor, hoist, lift)
import Pipes.Lift (distribute)
import Pipes.Internal (unsafeHoist)
import Control.Error (EitherT(..), runEitherT)

instance MFunctor (EitherT e) where
    hoist nat m = EitherT (nat (runEitherT m))

-- | Wrap the base monad in 'EitherT'
eitherP :: Monad m
        => Proxy a' a b' b m (Either e r)
        -> Proxy a' a b' b (EitherT e m) r
eitherP = unsafeHoist lift >=> lift . EitherT . return
{-# INLINABLE eitherP #-}

-- | Run 'EitherT' in the base monad
runEitherP :: Monad m
           => Proxy a' a b' b (EitherT e m) r
           -> Proxy a' a b' b m (Either e r)
runEitherP = runEitherT . distribute
{-# INLINABLE runEitherP #-}