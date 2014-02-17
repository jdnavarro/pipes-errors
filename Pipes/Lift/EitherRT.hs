{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pipes.Lift.EitherRT where

import Pipes (Proxy, MFunctor, lift, hoist)
import Pipes.Lift (distribute)
import Pipes.Internal (unsafeHoist)
import Control.Error
  ( EitherT(..)
  , EitherRT(..)
  , runEitherT
  , runEitherRT
  , flipET
  , flipE
  )
import Pipes.Lift.EitherT

instance MFunctor (EitherRT r) where
    hoist nat m = EitherRT (hoist nat (runEitherRT m))

-- | Turn 'EitherT' in the base monad into 'EitherRT'
eitherRP :: Monad m
         => Proxy a' a b' b (EitherT e m) r
         -> Proxy a' a b' b (EitherRT r m) e
eitherRP p = do
    x <- unsafeHoist lift $ runEitherP p
    lift . EitherRT . EitherT $ return x
{-# INLINABLE eitherRP #-}

-- | Turn 'EitherRT' in the base monad into 'EitherT'
runEitherRP :: Monad m
            => Proxy a' a b' b (EitherRT r m) e
            -> Proxy a' a b' b (EitherT e m) r
runEitherRP p = do
    x <- unsafeHoist lift . runEitherT . flipET . runEitherRT $ distribute p
    lift . EitherT . return $ flipE x
{-# INLINABLE runEitherRP #-}
